module HooglePlus.GHCChecker (
    runGhcChecks, parseStrictnessSig, checkStrictness', check) where

import Language.Haskell.Interpreter hiding (get)

import Types.Environment
import Types.Program
import Types.Type
import Types.Experiments
import Types.Filtering
import qualified Types.Common as TC (Id)
import Synquid.Type
import Synquid.Util hiding (fromRight)
import Synquid.Pretty as Pretty
import Database.Util
import HooglePlus.Utils
import HooglePlus.FilterTest (runChecks, parseTypeString)
import HooglePlus.Example

import Control.Exception
import Control.Monad.Trans
import Control.Monad (when)
import CorePrep
import CoreSyn
import Data.Data
import Data.Either
import Data.List (isInfixOf, isPrefixOf, intercalate)
import Data.List.Split (splitOn)
import Data.Maybe
import Data.Typeable
import Demand
import DmdAnal
import DynFlags
import FamInstEnv
import GHC
import GHC.Paths ( libdir )
import HscTypes
import IdInfo
import Outputable hiding (text, (<+>))
import qualified CoreSyn as Syn
import qualified Data.Map as Map hiding (map, foldr)
import qualified Data.Set as Set hiding (map)
import qualified Data.Text as T
import SimplCore (core2core)
import System.Directory (removeFile)
import Text.Printf
import Text.Regex
import Var
import Data.UUID.V4
import Control.Concurrent.Chan
import Control.Monad.Trans.State
import Control.Concurrent
import System.CPUTime (getCPUTime)
import Text.Read(readMaybe)


-- FIXME remove some?
import SymbolicMatch.Samples
import qualified SymbolicMatch.Expr as Expr
import qualified SymbolicMatch.Eval as Eval (eval)
import qualified SymbolicMatch.State as State (init)
import qualified SymbolicMatch.Match as Match (matchPairsPretty, MatchError(..))
import qualified Data.Bool (bool)
import System.IO (stderr, hPutStrLn)
import HooglePlus.LinearSynth (linearSynth)

showGhc :: (Outputable a) => a -> String
showGhc = showPpr unsafeGlobalDynFlags

ourFunctionName = "ghcCheckedFunction"

checkStrictness' :: Int -> String -> String -> [String] -> IO Bool
checkStrictness' tyclassCount  lambdaExpr typeExpr modules = GHC.runGhc (Just libdir) $ do
    tmpDir <- liftIO $ getTmpDir
    -- TODO: can we use GHC to dynamically compile strings? I think not
    let toModuleImportStr = (printf "import %s\n") :: String -> String
    let moduleImports = concatMap toModuleImportStr modules
    let sourceCode = printf "module Temp where\n%s\n%s :: %s\n%s = %s\n" moduleImports ourFunctionName typeExpr ourFunctionName lambdaExpr
    baseName <- liftIO $ nextRandom
    let baseNameStr = show baseName ++ ".hs"
    let fileName = tmpDir ++ "/" ++ baseNameStr
    liftIO $ writeFile fileName sourceCode

    -- Establishing GHC session
    env <- getSession
    dflags <- getSessionDynFlags
    let dflags' = (updOptLevel 2 dflags)    
    setSessionDynFlags $ dflags'

    -- Compile to core
    target <- guessTarget fileName Nothing
    setTargets [target]
    load LoadAllTargets

    modSum <- getModSummary $ mkModuleName "Temp"

    pmod <- parseModule modSum      -- ModuleSummary
    tmod <- typecheckModule pmod    -- TypecheckedSource
    dmod <- desugarModule tmod      -- DesugaredModule
    let core = coreModule dmod      -- CoreModule

    -- Run the demand analyzer
    -- prog is [<fooBinding>, <moduleBinding>]
    core' <- liftIO $ core2core env core
    prog <- liftIO $ (dmdAnalProgram dflags emptyFamInstEnvs $ mg_binds core')
    let decl = findOurBinding (prog :: [CoreBind]) -- only one method
    liftIO $ removeFile fileName
    -- liftIO $ printf "whole program: %s\n" $ showSDocUnsafe $ ppr $ prog
    -- TODO: I'm thinking of simply checking for the presence of `L` (lazy) or `A` (absent)
    -- on the singatures. That would be enough to show that the relevancy requirement is not met.

    case decl of
        NonRec id rest -> return $ isStrict tyclassCount decl
        _ -> error "checkStrictness: recursive expression found"
    
    where
        findOurBinding bs = head $ filter (\x-> ourFunctionName `isInfixOf` (showSDocUnsafe $ ppr x)) bs
        getStrictnessSig x = parseStrictnessSig $ showSDocUnsafe $ ppr $ x
        isStrict n x = let
            strictnessSig = getStrictnessSig x
            argStrictness = splitByArg strictnessSig
            -- do not care about the strictness of typeclass arguments
            restSigs = drop n argStrictness
            in not $ any (elem 'A') restSigs
        splitByArg :: String -> [String]
        splitByArg str = let
            regex = mkRegex "><"
            in splitRegex regex str

parseStrictnessSig :: String -> String
parseStrictnessSig result = let
    regex = mkRegex "Str=(<.*>)"
    in case (matchRegex regex result) of
        Just (match:_) -> match
        _ -> error $ "unable to find strictness in: " ++ result

checkStrictness :: Int -> String -> String -> [String] -> IO Bool
checkStrictness tyclassCount body sig modules =
    handle
        (\(SomeException _) -> return False)
        (checkStrictness' tyclassCount body sig modules)

check :: Goal -> SearchParams -> Chan Message -> Chan Message -> Maybe [Example] -> IO ()
check goal searchParams solverChan checkerChan examples = catch
    (evalStateT (check_ goal searchParams solverChan checkerChan examples) emptyFilterState)
    (\err ->
        writeChan checkerChan (MesgLog 0 "filterCheck" ("error: " ++ show err)) >>
        writeChan checkerChan (MesgClose (CSError err)))

check_ :: MonadIO m => Goal -> SearchParams -> Chan Message -> Chan Message -> Maybe [Example] -> FilterTest m ()
check_ goal searchParams solverChan checkerChan examples = do
    msg <- liftIO $ readChan solverChan
    handleMessages solverChan checkerChan (0.0 :: Double) msg
    return ()
    where
        handleMessages solverChan checkChan matchTime msg =
            case msg of
                (MesgP (program, stats, _)) -> do
                    -- executeCheck can return several programs given one 
                    -- program from the petri net (different lambdas)
                    (progs, matchTime') <- executeCheck program 
                    state <- get
                    let matchTime'' = matchTime + matchTime'
                    let stats' = stats {matchTime = matchTime''}
                    let next = (liftIO . readChan) solverChan >>= handleMessages solverChan checkerChan matchTime''
                    mapM_ (\c -> bypass (MesgP (c, stats', state))) progs
                    next
                (MesgClose _) -> bypass msg
                _ -> (bypass msg) >> (liftIO . readChan) solverChan >>= handleMessages solverChan checkerChan matchTime

            where
                bypass message = liftIO $ writeChan checkerChan message

        (env, destType) = preprocessEnvFromGoal goal
        -- returns the program with symbols replaced
        executeCheck prog = do
            case examples of
                Nothing -> do
                    ghcCheck <- runGhcChecks searchParams env destType prog checkerChan
                    return $ (maybeToList ghcCheck, 0.0)
                Just [] -- when examples flags exists but is [], the symbols exist and won't be replaced
                        -- do not wast time on GHC
                    | hasSymbol prog -> do
                        liftIO $ writeChan checkerChan (MesgLog 1 "filterCheck" ("Test \'" ++ show prog ++ "\': rejected (symbol not replaced)."))
                        return ([], 0.0)
                    | otherwise -> do 
                        ghcCheck <- runGhcChecks searchParams env destType prog checkerChan
                        return $ (maybeToList ghcCheck, 0.0)
                Just examples' -> do
                    start <- liftIO getCPUTime
                    progs <- runExampleChecks searchParams env destType prog examples' checkerChan
                    end <- liftIO getCPUTime
                    let diff = fromIntegral (end - start) / (10^12)
                    let progs' = filter (not . hasSymbol) progs
                    case progs' of
                        []  -> return ([], diff)
                        _:_ -> do
                            ghcChecks <- mapM (\p -> runGhcChecks searchParams env destType p checkerChan) progs'
                            return $ (catMaybes ghcChecks, diff)
            where
                hasSymbol :: UProgram -> Bool
                hasSymbol prog = case content prog of
                    PSymbol id -> any (\(s, _) -> s `isInfixOf` id) symbolsInfo
                    PApp id args -> 
                        any (\(s, _) -> s `isInfixOf` id) symbolsInfo || any hasSymbol args
                    _ -> error "Solution not expexted to have other than PSym and PApp."
                
-- validate type signiture, run demand analysis, and run filter test
-- checks the end result type checks; all arguments are used; and that the program will not immediately fail
runGhcChecks :: MonadIO m => SearchParams -> Environment -> RType -> UProgram -> Chan Message -> FilterTest m (Maybe UProgram)
runGhcChecks params env goalType prog checkerChan = let
     -- constructs program and its type signature as strings
    (modules, funcSig, body, argList) = extractSolution env goalType prog
    tyclassCount = length $ Prelude.filter (\(id, _) -> tyclassArgBase `isPrefixOf` id) argList
    expr = body ++ " :: " ++ funcSig
    disableDemand = _disableDemand params
    disableFilter = _disableFilter params
    in do -- when there are no arguments, the strictness is unnecesssary and raises error
        typeCheckResult <- if disableDemand then return (Right True) else liftIO $ runInterpreter $ checkType expr modules
        strictCheckResult <- if disableDemand || null argList then return True else liftIO $ checkStrictness tyclassCount body funcSig modules
        filterCheckResult <- if disableFilter then return True else runChecks body funcSig modules
        case typeCheckResult of
            Left err -> do 
                liftIO $ writeChan checkerChan (MesgLog 1 "exampleCheck" ("Test \'" ++ show prog ++ "\': rejected by GHC: type check."))
                liftIO $ hPutStrLn stderr (displayException err) 
                return Nothing
            Right False -> do
                liftIO $ writeChan checkerChan (MesgLog 1 "exampleCheck" ("Test \'" ++ show prog ++ "\': rejected by GHC: type check."))
                liftIO $ putStrLn "Program does not typecheck" 
                return Nothing
            Right True
                | strictCheckResult -> do 
                    if filterCheckResult
                        then do
                            liftIO $ writeChan checkerChan (MesgLog 1 "exampleCheck" ("Test \'" ++ show prog ++ "\': accepted."))
                            return $ Just prog
                        else do 
                            liftIO $ writeChan checkerChan (MesgLog 1 "exampleCheck" ("Test \'" ++ show prog ++ "\': rejected by GHC: filter check."))
                            return Nothing
                | otherwise -> do
                    liftIO $ writeChan checkerChan (MesgLog 1 "exampleCheck" ("Test \'" ++ show prog ++ "\': rejected by GHC: dependency analysis."))
                    return Nothing

-- run the match algorithm against an example and return
-- new programs with the symbols replaced
runExampleChecks :: MonadIO m 
                 => SearchParams 
                 -> Environment 
                 -> RType 
                 -> UProgram 
                 -> [Example]
                 -> Chan Message
                 -> FilterTest m [UProgram]
runExampleChecks params env goalType prog examples checkerChan = do
    if (not . checkFunctionsPresent) prog 
        then do
            liftIO $ writeChan checkerChan (MesgLog 1 "exampleCheck" ("Test \'" ++ show prog ++ "\': rejected by match (functions not supported by Match)."))
            return []
        else case Match.matchPairsPretty 200 pairs functionsEnv of
            Left err -> 
                case err of 
                    Match.Exception msg -> do 
                        liftIO $ writeChan checkerChan (MesgLog 1 "exampleCheck" ("Test \'" ++ show prog ++ "\': rejected by match (exception on match: " ++ msg ++ ")."))
                        return []
                    Match.Mismatch -> do
                        liftIO $ writeChan checkerChan (MesgLog 1 "exampleCheck" ("Test \'" ++ show prog ++ "\': rejected by match (mismatch)."))
                        return []
                    Match.DepthReached -> do
                        liftIO $ writeChan checkerChan (MesgLog 1 "exampleCheck" ("Test \'" ++ show prog ++ "\': rejected by match (max depth reached)."))
                        return []
            Right cs -> do
                -- replace all non-function symbols built by match
                let prog'' = replaceSymsInProg cs progWithGoodSymNames
                
                -- get symbols of the remaining symbols (functions) if any
                let (_, _, lambda, _) = extractSolution env goalType prog''
                let lambda' = T.unpack $ T.replace (T.pack "Sym") (T.pack "_Sym") (T.pack lambda)
                let expr = lambda' ++ " :: " ++ funcSig
                mbsts <- getHolesTypes expr modules "Sym"
                
                case mbsts of
                    -- error getting symbols types, probably compilation error
                    Nothing -> do
                        liftIO $ writeChan checkerChan (MesgLog 1 "exampleCheck" ("Test \'" ++ show prog ++ "\': rejected by match (compilation)"))
                        return []
                    Just [] -> do
                        prog''' <- replaceWilcards prog''
                        case prog''' of
                            Just prog'''' -> return [prog'''']
                            Nothing -> do
                                liftIO $ writeChan checkerChan (MesgLog 1 "exampleCheck" ("Test \'" ++ show prog ++ "\': rejected by match (compilation)"))
                                return []
                    Just sts -> do
                        -- if any symbol is not a function, it should be replace by match, before...
                        when (not (allFunTys sts)) $ error "Non function symbol found after match"
                        -- FIXME: should set seed for widlcards in lams, otherwise may colide
                        -- synthesize lambdas for the function symbols and replace
                        synthRes <- liftIO $ synthLambdas (_symsToLinearSynth env) sts argList cs
                        case synthRes of
                            [] -> do
                                liftIO $ writeChan checkerChan (MesgLog 1 "exampleCheck" ("Test \'" ++ show prog ++ "\': rejected by match (synthesizing lambdas)."))
                                return []
                            lams@(_:_) -> do
                                let progsWithLams = [replaceLamsInProg l prog'' | l <- lams]
                                mbsProgs <- mapM replaceWilcards progsWithLams
                                return $ catMaybes mbsProgs
    where
        (modules, funcSig, _, argList) = extractSolution env goalType prog

        progWithGoodSymNames :: UProgram
        progWithGoodSymNames = case removeTcargs (changeSymbolsNames $ removeTc prog) of
            Just prog -> prog
            Nothing -> error "prog is simply a tcarg"

        -- SymbolicMatch does not support all functions, and raises an error
        -- we prefer to avoid error and skip to the next
        checkFunctionsPresent :: UProgram -> Bool
        checkFunctionsPresent Program {content = (PSymbol _)} = True -- FIXME lambda functions as arguments
        checkFunctionsPresent Program {content = (PApp id args)}
            | isJust $ lookupFun id = all checkFunctionsPresent args
            | otherwise = False
        
        pairs :: [(Expr.Expr, Expr.Expr)]
        pairs = let argsNames = map fst argList in 
            map (\ex -> (programToExpr progWithGoodSymNames ex argsNames, output ex)) examples

        -- true if all the strings represent function types
        allFunTys :: [(Int, String)] -> Bool
        allFunTys sts = all (\(_, t) -> T.pack "->" `T.isInfixOf` T.pack t) sts
        
        -- The symbols in prog are replaced by the result of match
        -- also, when arguments of functions are @@ (typeclass instances)
        replaceSymsInProg :: [(Int, [Expr.Expr], Expr.Expr)] -> UProgram -> UProgram
        replaceSymsInProg cs prog = case content prog of
            PSymbol id 
                | "Sym" `isPrefixOf` id ->  let
                    symInd = read (drop (length "Sym") id) :: Int
                    repl = lookup symInd cs' in
                        case repl of
                            Nothing -> prog -- it should be a function
                            Just repl'
                                | Expr.needsPar repl' -> prog {content = PSymbol $ "(" ++ Expr.showExpr functionsNames repl' ++ ")"}
                                | otherwise -> prog {content = PSymbol $ Expr.showExpr functionsNames repl'}
                | otherwise -> prog
            PApp id args -> prog {content = PApp id (map (replaceSymsInProg cs) args)}
            _ -> error "Solution not expexted to have other than PSym and PApp."
            where
                -- we only want to replace non functions values
                cs' = mapMaybe (\(s, as, v) -> case as of {[] -> Just (s, v); _:_ -> Nothing}) cs
        
        -- replace lambdas synthesized by linearSynth
        replaceLamsInProg :: [(Int, String)] -> UProgram -> UProgram
        replaceLamsInProg lams prog = case content prog of
            PSymbol id 
                | "Sym" `isPrefixOf` id ->  let
                    symInd = read (drop (length "Sym") id) :: Int in
                    case lookup symInd lams of
                        Nothing -> error $ "Symbol " ++ id ++ " is not assigned in lambdas."
                        Just lam -> prog {content = PSymbol $ lam}
                | otherwise -> prog
            PApp id args -> prog {content = PApp id (map (replaceLamsInProg lams) args)}
            _ -> error "Solution not expexted to have other than PSym and PApp."

        -- remove typeclass arguments
        removeTc :: UProgram -> UProgram
        removeTc p = case content p of
            PSymbol _ -> p
            PApp id args -> let args' = filter (not . isPSymWithTc) args in
                p {content = PApp id args'}
            where 
                isPSymWithTc :: UProgram -> Bool
                isPSymWithTc p = case content p of 
                    PSymbol id
                        | "@@" `isPrefixOf` id -> True
                        | otherwise -> False
                    _ -> False

        -- given an expression fun :: sig with holes, return its types as strings
        getHolesTypes :: MonadIO m 
                      => String          -- en expression whose wildcard names are _<prefix><n> with signature
                      -> [String]        -- modules
                      -> String          -- prefix of holes; example _h1 -> h
                      -> FilterTest m (Maybe [(Int, String)]) -- for each symbol, a pair with index and type
        getHolesTypes expr modules prefix = do
            res <- liftIO $ runInterpreter $ checkType expr modules
            case res of 
                Left err -> case err of
                    WontCompile msgs -> let msgAsTexts = map (\(GhcError m) -> T.pack m) msgs in
                        if all (\m -> T.pack "Found hole" `T.isInfixOf` m) msgAsTexts
                            then return $ Just $ mapMaybe (extractType (T.pack prefix)) msgAsTexts
                            else trace ("Error: " ++ show msgs) $  return Nothing
                    _ -> trace "getHolesTypes: Expected error message to be WontCompile" $ return Nothing
                Right _ -> return (Just [])

        -- extracts the hole type from the "found hole" message
        -- each hole has name _<prefix><n>
        -- nothing means that the msg does not have a hole with that prefix
        extractType :: T.Text -> T.Text -> Maybe (Int, String)
        extractType prefix msg = let
            -- holeLine: Found hole: _Sym0 :: Int -> Int -> Int
            holeLine = (T.lines msg) !! 1 
            -- hole: _Sym0 :: Int -> Int -> Int
            hole = snd $ T.break (== '_') holeLine
            -- symbolName: _Sym0
            symbolName = head $ T.words hole
            -- symbolType: Int -> Int -> Int
            symbolType = T.unwords $ drop 2 $ T.words hole in
                case T.stripPrefix (T.cons '_' prefix) symbolName of
                    Just t -> case readMaybe (T.unpack t) :: Maybe Int of
                        Just int -> Just (int, T.unpack symbolType)
                        Nothing -> error "extractType: error reading type"
                    Nothing -> Nothing

        synthLambdas :: [(String, FunctionSignature, Int)]
                     -> [(Int, String)] 
                     -> [(TC.Id, RSchema)] 
                     -> [(Int, [Expr.Expr], Expr.Expr)]
                     -> IO [[(Int, String)]]
        synthLambdas env sts argsList cs = case sts of
            [] -> return []
            ((si, st):sts)
                | (not . null) sts -> error "more than 1 lam to synthesize"
                | otherwise -> do
                    lams <- linearSynth env st argsList (matchFn si) nextSym
                    return $ sequence [map (\l -> (si, l)) lams]
            where
                nextSym = (maximum $ Expr.symbols $ fst $ head $ pairs) + 1 

                matchFn :: Int -> Expr.Expr -> Either Match.MatchError [(Int, [Expr.Expr], Expr.Expr)]
                matchFn si lam = let
                    pairs' = map (\(src, dst) -> (Expr.replace src (Expr.Sym si) lam, dst)) pairs in
                        trace ("Pairs: " ++ show pairs') $ Match.matchPairsPretty 350 pairs' functionsEnv
        
        -- try replace wildcard generated by match with default values
        replaceWilcards :: MonadIO m => UProgram -> FilterTest m (Maybe UProgram)
        replaceWilcards p = do
            let (modules, funcSig, lambda, _) = extractSolution env goalType p
            let expr = printf "%s :: %s" lambda funcSig
            mbwts <- getHolesTypes expr modules ""
            case mbwts of
                -- error getting holes types due to compilation error
                Nothing -> return Nothing
                Just [] -> return $ Just p 
                Just wts -> let defaults = map (\(i, t) -> (i, def (_returnType $ parseTypeString t))) wts in
                    return $ Just $ replaceWildsInProg defaults p
            where
                -- FIXME _hole inside id (text; use replace)
                replaceWildsInProg :: [(Int, String)] -> UProgram -> UProgram
                replaceWildsInProg wilds prog = aux prog
                    where
                    holes :: [(T.Text, T.Text)]
                    holes = map (\(hi, hd) -> (T.pack ('_':show hi), T.pack hd)) wilds

                    aux :: UProgram -> UProgram
                    aux prog = case content prog of
                        PSymbol id -> let id' = foldr (\(hole, def) id'' -> T.replace hole def id'') (T.pack id) holes in
                            prog {content = PSymbol (T.unpack id')}
                        PApp id args -> prog {content = PApp id (map aux args)}
                        _ -> error "Solution not expexted to have other than PSym and PApp."

                -- get a default value for each type as a string
                def :: ArgumentType -> String
                def (Concrete "Int") = "0"
                def (Concrete "Integer") = "0"
                def (Concrete "Bool") = "False"
                def (ArgTypeApp (Concrete "Maybe") _) = "Nothing"
                def (ArgTypeApp (Concrete "Either") t) = "(Left " ++ def t ++ ")"
                def (ArgTypeList _) = "[]"
                def (ArgTypeTuple ts) = let defs = map def ts in 
                    "(" ++ intercalate ", " defs ++ ")"
                def (Polymorphic name) = "undefined"
                def t = error $ "Default value missing for type " ++ show t

-- ensures that the program type-checks
checkType :: String -> [String] -> Interpreter Bool
checkType expr modules = do
    setImports modules
    -- Ensures that if there's a problem we'll know
    Language.Haskell.Interpreter.typeOf expr
    typeChecks expr 