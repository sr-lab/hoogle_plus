module HooglePlus.GHCChecker (
    runGhcChecks, parseStrictnessSig, checkStrictness', check) where

import Language.Haskell.Interpreter hiding (get)

import Types.Environment
import Types.Program
import Types.Type
import Types.Experiments
import Types.Filtering
import Synquid.Type
import Synquid.Util hiding (fromRight)
import Synquid.Pretty as Pretty
import Database.Util
import HooglePlus.Utils
import HooglePlus.FilterTest (runChecks)
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
import qualified Data.Text as Text
import SimplCore (core2core)
import System.Directory (removeFile)
import Text.Printf
import Text.Regex
import Var
import Data.UUID.V4
import Control.Concurrent.Chan
import Control.Monad.Trans.State
import Control.Concurrent

-- FIXME remove some?
import SymbolicMatch.Samples
import qualified SymbolicMatch.Expr as Expr
import qualified SymbolicMatch.Eval as Eval (eval)
import qualified SymbolicMatch.State as State (init)
import qualified SymbolicMatch.Match as Match (matchExprsPretty, MatchError(..))
import qualified Data.Bool (bool)
import System.IO (stderr, hPutStrLn)

showGhc :: (Outputable a) => a -> String
showGhc = showPpr unsafeGlobalDynFlags

ourFunctionName = "ghcCheckedFunction"

checkStrictness' :: Int -> String -> String -> [String] -> IO Bool
checkStrictness' tyclassCount  lambdaExpr typeExpr modules = GHC.runGhc (Just libdir) $ do
    tmpDir <- liftIO $ getTmpDir
    -- TODO: can we use GHC to dynamically compile strings? I think not
    let modules' = filter (/= "Symbol") modules
    let toModuleImportStr = (printf "import %s\n") :: String -> String
    let moduleImports = concatMap toModuleImportStr modules'
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

check :: Goal -> SearchParams -> Chan Message -> Chan Message -> Example -> IO ()
check goal searchParams solverChan checkerChan example = catch
    (evalStateT (check_ goal searchParams solverChan checkerChan example) emptyFilterState)
    (\err ->
        writeChan checkerChan (MesgLog 0 "filterCheck" ("error: " ++ show err)) >>
        writeChan checkerChan (MesgClose (CSError err)))

check_ :: MonadIO m => Goal -> SearchParams -> Chan Message -> Chan Message -> Example -> FilterTest m ()
check_ goal searchParams solverChan checkerChan example = do
    msg <- liftIO $ readChan solverChan
    handleMessages solverChan checkerChan msg
    return ()
    where
        handleMessages solverChan checkChan msg =
            case msg of
                (MesgP (program, stats, _)) -> do
                    checkResult <- executeCheck program
                    state <- get
                    case checkResult of
                        Just program' -> (bypass (MesgP (program', stats, state))) >> next 
                        Nothing -> next
                (MesgClose _) -> bypass msg
                _ -> (bypass msg) >> next

            where
                next = (liftIO . readChan) solverChan >>= handleMessages solverChan checkerChan
                bypass message = liftIO $ writeChan checkerChan message

        (env, destType) = preprocessEnvFromGoal goal
        -- returns the program with symbols replaced
        executeCheck prog = do
            ghcChecks <- runGhcChecks searchParams env destType prog
            if ghcChecks 
                then do
                    exampleChecks <- return $ Just prog -- runExampleChecks searchParams env destType prog example
                    case exampleChecks of 
                        Just prog' -> do
                            liftIO $ putStrLn $ "Test \'" ++ show prog' ++ "\': accepted."
                            return exampleChecks
                        Nothing -> do
                            liftIO $ putStrLn $ "Test \'" ++ show prog ++ "\': rejected by match."
                            return Nothing
                else return Nothing

-- validate type signiture, run demand analysis, and run filter test
-- checks the end result type checks; all arguments are used; and that the program will not immediately fail
runGhcChecks :: MonadIO m => SearchParams -> Environment -> RType -> UProgram -> FilterTest m Bool
runGhcChecks params env goalType prog  = let
    -- remove module prefix from prog (Symbol.)
    prog' = removeProgModulePrefix prog
    -- constructs program and its type signature as strings
    (modules, funcSig, body, argList) = extractSolution env goalType prog'
    tyclassCount = length $ Prelude.filter (\(id, _) -> tyclassArgBase `isPrefixOf` id) argList
    -- all the used symbols in prog
    symbols = Set.toList $ usedSymbols prog'
    -- body where the used symbols are also parameters
    -- in order to typecheck and compile
    body' = addSymbolLets body symbols
    expr = body' ++ " :: " ++ funcSig
    disableDemand = _disableDemand params
    disableFilter = _disableFilter params
    -- Symbol is only for generating the database, 
    -- not supposed to be used to compile the solutions
    modules' = filter (/= "Symbol") modules 
    in do
        typeCheckResult <- if disableDemand then return (Right True) else liftIO $ runInterpreter $ checkType expr modules'
        case trace ("Expr==" ++ expr) typeCheckResult of
            Left err -> do 
                liftIO $ putStrLn $ "Test \'" ++ show prog ++ "\': rejected by GHC: type check."
                liftIO $ hPutStrLn stderr (displayException err) 
                return False
            Right False -> do
                liftIO $ putStrLn $ "Test \'" ++ show prog ++ "\': rejected by GHC: type check."
                liftIO $ putStrLn "Program does not typecheck" 
                return False
            Right True -> return True
                -- FIXME: dependency analysis & filter checks does not work with symbol defined as undefined
                -- dependency analysis is rejecting drop symbol arg1
                {-strictCheckResult <- if disableDemand then return True else liftIO $ checkStrictness tyclassCount body' funcSig modules'
                if strictCheckResult 
                    then do 
                        filterCheckResult <- if disableFilter then return True else runChecks body' funcSig modules'
                        when (not filterCheckResult) $ liftIO $ putStrLn $ "Test \'" ++ show prog ++ "\': rejected by GHC: filter check."
                        return True --filterCheckResult
                    else do
                        liftIO $ putStrLn $ "Test \'" ++ show prog ++ "\': rejected by GHC: dependency analysis."
                        return False-}

    where
        -- returns all the used symbols
        usedSymbols :: UProgram -> Set.Set String
        usedSymbols prog = 
            case content prog of
                (PSymbol id)
                    | isSymbol id -> Set.singleton id
                    | otherwise -> Set.empty
                (PApp id args) -> let
                    sArgs = foldr (\c r -> Set.union (usedSymbols c) r) Set.empty args in
                        if isSymbol id then Set.insert id sArgs else sArgs
                _ -> error "Solution expected to have only PSym and PApp"

        -- add symbols in let so that the program compiles
        addSymbolLets :: String -> [String] -> String
        addSymbolLets lambda symbols = let
            letBindings = foldr (\c r -> printf "%s = undefined; %s" c r) "" symbols in
                printf "let %s in %s" letBindings lambda

        -- remove module prefix of all occurences of symbols (Symbol.symbolInt -> symbolInt)
        removeProgModulePrefix :: UProgram -> UProgram
        removeProgModulePrefix p = case content p of
            PSymbol id
                | isSymbolWithPrefix id -> p {content = PSymbol (removeModulePrefix id)}
                | otherwise -> p
            PApp id args -> let
                id' = if isSymbolWithPrefix id then removeModulePrefix id else id
                args' = map removeProgModulePrefix args in
                    p {content = PApp id' args'}

-- run the match algorithm against an example and return a new program with the symbols replaced
runExampleChecks :: MonadIO m => SearchParams -> Environment -> RType -> UProgram -> Example -> FilterTest m (Maybe UProgram)
runExampleChecks params env goalType prog example = do 
    let (_, _, body, argList) = extractSolution env goalType prog
    let argsNames = map fst argList
    let progWithoutTc = removeTc prog
    let (prog', expr) = programToExpr progWithoutTc example argsNames
    case trace ("Call Match: expr = " ++ Expr.showExpr functionsNames expr ++ ", dst = " ++ Expr.showExpr functionsNames (output example)) $ Match.matchExprsPretty 150 expr functionsEnv (output example) of
        Left err
            | Match.Exception msg <- err -> liftIO (putStrLn $ "match: " ++ msg) >> return Nothing
            | otherwise -> return Nothing
        Right cs -> return $ Just $ replaceSymsInProg cs prog'
    where
        -- The symbols in prog are replaced by the result of match
        -- also, when arguments of functions are @@ (typeclass instances)
        replaceSymsInProg :: [(Int, [Expr.Expr], Expr.Expr)] -> UProgram -> UProgram
        replaceSymsInProg cs prog = case content prog of
            PSymbol id 
                | "Sym" `isPrefixOf` id ->  let
                    symInd = read (drop (length "Sym") id) :: Int
                    repl = lookup symInd cs' in
                        case repl of
                            Nothing -> error $ "Symbol " ++ id ++ " is not assigned in cs."
                            Just repl'
                                | Expr.needsPar repl' -> prog {content = PSymbol $ "(" ++ Expr.showExpr functionsNames repl' ++ ")"}
                                | otherwise -> prog {content = PSymbol $ Expr.showExpr functionsNames repl'}
                | otherwise -> prog
            PApp id args -> prog {content = PApp id (map (replaceSymsInProg cs) args)}
            _ -> error "Solution not expexted to have other than PSym and PApp."
            where
                cs' = map (\(a, _, b) -> (a, b)) cs -- we do not have functions here, so do not need args
        
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


-- ensures that the program type-checks
checkType :: String -> [String] -> Interpreter Bool
checkType expr modules = do
    setImports modules
    -- Ensures that if there's a problem we'll know
    Language.Haskell.Interpreter.typeOf expr
    typeChecks expr 
