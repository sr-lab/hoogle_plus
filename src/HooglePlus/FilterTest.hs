{-# LANGUAGE FlexibleContexts, LambdaCase, NamedFieldPuns #-}
module HooglePlus.FilterTest where

import qualified Data.Map as Map
import qualified Hoogle
import qualified Language.Haskell.Interpreter as LHI
import qualified Test.QuickCheck as QC

import Language.Haskell.Interpreter (InterpreterT, InterpreterError(..), Extension(..), OptionVal(..))
import Control.Monad.Extra (andM)

import Control.Exception
import Control.Monad
import Control.Monad.State
import Data.Functor ((<&>))
import Data.Bifunctor (second)
import Data.Either
import Data.List
import Data.Maybe
import Data.Typeable
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Syntax
import System.Timeout
import Text.Printf
import Data.Containers.ListUtils (nubOrd)
import Data.List.Extra (splitOn)
import Data.UUID.V4 (nextRandom)

import HooglePlus.Utils (splitConsecutive, extractSolution, printFilter)
import Paths_HooglePlus
import Synquid.Type
import Types.Environment
import Types.Filtering
import Types.Program
import Types.Type hiding (typeOf)
import PetriNet.Utils (unHTML)
import Synquid.Utils (getTmpDir)


import Debug.Trace

-- | Parse a string to an internal representation of type signatures.
-- >>> parseTypeString "(Eq a, Ord b) => a -> b"
-- (((Eq) (a)), ((Ord) (b))) => a -> b
-- >>> _returnType $ parseTypeString "((a -> a))"
-- ((a) -> (a))
parseTypeString :: String -> FunctionSignature
parseTypeString input = FunctionSignature constraints argsType returnType
  where
    (constraints, argsType, returnType) = buildSig [] [] value
    (ParseOk value) = parseType input

    buildSig constraints argList (TyForall _ _ (Just ctx) t) = buildSig constraints' argList t
      where constraints' = constraints ++ extractConstraints constraints ctx
    buildSig constraints argList (TyFun _ typeArg typeRet) = buildSig constraints argList' typeRet
      where argList' = argList ++ [extractType typeArg]
    buildSig constraints argList typeRet = (constraints, argList, extractType typeRet)

    extractType (TyVar _ (Ident _ name)) = Polymorphic name
    extractType (TyCon _ (UnQual _ (Ident _ name))) = Concrete name
    extractType (TyCon _ (Qual _ (ModuleName _ moduleName) (Ident _ id))) = Concrete (printf "%s.%s" moduleName id)
    extractType (TyList _ arg) = ArgTypeList (extractType arg)
    extractType (TyParen _ t) = extractType t
    extractType (TyApp _ l r) = ArgTypeApp (extractType l) (extractType r)
    extractType (TyTuple _ _ types) = ArgTypeTuple (map extractType types)
    extractType (TyFun _ src dst) = ArgTypeFunc (extractType src) (extractType dst)
    extractType other = throw $ NotSupportedException ("Type Parser: unsupported " ++ show other)

    extractQualified (TypeA _ t) = extractType t
    extractQualified (ParenA _ qual) = extractQualified qual
    extractQualified other = throw $ NotSupportedException ("Type Parser: unsupported " ++ show other)

    extractConstraints constraints (CxSingle _ item) = constraints ++ [extractQualified item]
    extractConstraints constraints (CxTuple _ list) = foldr ((++) . (:[]) . extractQualified) constraints list

-- | instantiate polymorphic types in function signature with `Int`
-- >>> instantiateSignature (parseTypeString "[a] -> [b] -> (a -> b -> c) -> (c, Maybe c)")
-- () => Int -> String -> ((Int) -> (((String) -> (Bool)))) -> (Bool, ((Maybe) (Bool)))
instantiateSignature :: FunctionSignature -> FunctionSignature
instantiateSignature (FunctionSignature _ argsType returnType) =
    let mappings = buildMappings (returnType:argsType) in
    let instantiate = instantPolymorphic mappings in
      FunctionSignature [] (map instantiate argsType) (instantiate returnType)
  where
    buildMappings xs = zip (sort $ nubOrd $ concatMap capturePolymorphic xs) types
      where types = ["Int", "String", "Bool"]

    capturePolymorphic = \case
      Polymorphic   x   -> [x];
      ArgTypeList   x   -> capturePolymorphic x
      ArgTypeTuple  xs  -> concatMap capturePolymorphic xs
      ArgTypeApp    l r -> concatMap capturePolymorphic [l, r]
      ArgTypeFunc   l r -> concatMap capturePolymorphic [l, r]
      _                 -> []

    instantPolymorphic mappings = f
      where f = \case
                  Polymorphic   x   -> Instantiated $ maybe "Int" snd (find ((== x) . fst) mappings)
                  ArgTypeList   x   -> ArgTypeList $ f x
                  ArgTypeTuple  xs  -> ArgTypeTuple $ map f xs
                  ArgTypeApp    l r -> ArgTypeApp (f l) (f r)
                  ArgTypeFunc   l r -> ArgTypeFunc (f l) (f r)
                  other             -> other

replaceMyType :: ArgumentType -> ArgumentType
replaceMyType x =
  let
    apply a b = ArgTypeApp (ArgTypeApp (Concrete "MyFun") a) b
    applyConcrete t = case t of
                      "Int"     -> "MyInt"
                      "Char"    -> "MyChar"
                      "String"  -> "[MyChar]"
                      _         -> t
  in case x of
    Concrete      n         -> Concrete $ applyConcrete n
    Polymorphic   _         -> x
    Instantiated  n         -> ArgTypeApp (Concrete "Box") (Concrete $ applyConcrete n)
    ArgTypeList   t         -> ArgTypeList (replaceMyType t)
    ArgTypeTuple  ts        -> ArgTypeTuple (map replaceMyType ts)
    ArgTypeApp    f a       -> ArgTypeApp (replaceMyType f) (replaceMyType a)
    ArgTypeFunc   arg res   -> apply (replaceMyType arg) (replaceMyType res)


buildFunctionWrapper :: [(String, String)] -> FunctionSignature -> (String, String, String, String) -> String
buildFunctionWrapper functions solutionType@FunctionSignature{_returnType} params@(plain, typed, shows, unwrp) =
    unwords
      (map (buildLetFunction $ show solutionType) functions ++ [buildWrapper (map fst functions) params (show _returnType)])
  where
    buildLetFunction :: String -> (String, String) -> String
    buildLetFunction programType (wrapperName, program) =
      printf "let %s = ((%s) :: %s) in" wrapperName program programType :: String

    -- ! the wrapper magic (i.e. MyInt) only lives here (inside `typed`)
    buildWrapper :: [String] -> (String, String, String, String) -> String -> String
    buildWrapper wrapperNames (plain, typed, shows, unwrp) retType =
      printf "let executeWrapper %s = (Prelude.map (\\f -> f %s :: %s) [%s]) in" typed unwrp retType (intercalate ", " wrapperNames) :: String

buildNotCrashProp :: String -> FunctionSignature -> String
buildNotCrashProp solution funcSig = formatNotCrashProp params wrapper
  where
    params@(plain, typed, shows, unwrp) = showParams (_argsType funcSig)

    wrapper = buildFunctionWrapper [("wrappedSolution", solution)] funcSig params
    formatNotCrashProp = formatProp "prop_not_crash" "\\out -> (not $ isFailedResult $ Prelude.head out) ==> True"

    formatProp propName propBody (plain, typed, shows, unwrp) wrappedSolution = unwords
      [ wrappedSolution
      , printf "let %s %s = monadicIO $ run $ labelEvaluation (%s) (executeWrapper %s) (%s) in" propName plain shows plain propBody
      , printf "quickCheckWithResult defaultTestArgs %s" propName ] :: String

buildDupCheckProp :: (String, [String]) -> FunctionSignature -> [String]
buildDupCheckProp (sol, otherSols) funcSig =
  map (\x -> buildDupCheckProp' (sol, [x]) funcSig) otherSols

buildDupCheckProp' :: (String, [String]) -> FunctionSignature -> String
buildDupCheckProp' (sol, otherSols) funcSig = unwords [wrapper, formatProp]
  where
    params@(plain, typed, shows, unwrp) = showParams (_argsType funcSig)
    solutionType = show funcSig

    wrapper = buildFunctionWrapper solutions funcSig params
    solutions = zip [printf "result_%d" x :: String | x <- [0..] :: [Int]] (sol:otherSols)

    formatProp = unwords
      [ printf "let prop_duplicate %s = monadicIO $ run $ labelEvaluation (%s) (executeWrapper %s) (\\out -> (not $ anyDuplicate out) ==> True) in" plain shows plain
      , printf "quickCheckWithResult defaultTestArgs prop_duplicate" ] :: String

-- | Run Hint with the default script loaded.
runInterpreterWithEnvTimeout :: Int -> InterpreterT IO a -> IO (Either InterpreterError a)
runInterpreterWithEnvTimeout timeInMicro = runInterpreterWithEnvTimeoutPrepareModule timeInMicro (sequence ioList)
  where ioList = [getDataFileName "InternalTypeGen.hs"]

-- | Run Hint with the default script loaded, but also prepare arguments for HOFs.
runInterpreterWithEnvTimeoutHOF :: MonadIO m => Int -> FunctionSignature -> InterpreterT IO a -> FilterTest m (Either InterpreterError a)
runInterpreterWithEnvTimeoutHOF timeInMicro funcSig executeInterpreter = do
    fileName <- prepareEnvironment funcSig

    let ioList = [getDataFileName "InternalTypeGen.hs", pure fileName]
    result <- liftIO $ runInterpreterWithEnvTimeoutPrepareModule timeInMicro (sequence ioList) executeInterpreter
    case result of
      Left (WontCompile e)  -> liftIO $ print e >> runInterpreterWithEnvTimeout timeInMicro executeInterpreter
      _                     -> return result

-- | Prepare all scripts, get the paths of scripts, and run Hint.
runInterpreterWithEnvTimeoutPrepareModule :: Int -> IO [String] -> InterpreterT IO a -> IO (Either InterpreterError a)
runInterpreterWithEnvTimeoutPrepareModule timeInMicro prepareModule executeInterpreter =
  mergeTimeout <$> timeout timeInMicro (prepareModule >>= runInterpreter)
  where
    runInterpreter moduleFiles = LHI.runInterpreter $ do
      LHI.set [LHI.languageExtensions := [ExtendedDefaultRules, ScopedTypeVariables]]
      LHI.loadModules moduleFiles
      LHI.getLoadedModules >>= LHI.setTopLevelModules
      executeInterpreter

    mergeTimeout = \case
      Just v  -> v
      Nothing -> Left $ NotAllowed "timeout"
      -- Hint only throws NotAllowed when setting a non-existing top-level module.
      -- We never had such cases and can safely steal NotAllowed to represent timeout. 

-- | Initiate an interpreter with context, evaluate the given property, and return the backend result. 
-- todo: refactor 'funcSig' out
evaluateProperties :: MonadIO m => [String] -> FunctionSignature -> [String] -> FilterTest m (Either InterpreterError [BackendResult])
evaluateProperties modules funcSig properties =
  runInterpreterWithEnvTimeoutHOF defaultInterpreterTimeoutMicro funcSig $ do
    LHI.setImportsQ (zip modules (repeat Nothing) ++ frameworkModules)
    mapM (\prop -> LHI.interpret prop (LHI.as :: IO BackendResult) >>= liftIO) properties

validateCandidate :: MonadIO m => [String] -> Candidate -> FunctionSignature -> FilterTest m CandidateValidDesc
validateCandidate modules solution funcSig = do
    result <- evaluateProperties modules funcSig [prop]
    case result of
      Left  (NotAllowed _)            -> return $ Unknown "timeout"
      Left  error                     -> return $ Unknown $ show error
      Right [result]                  -> return $ readResult result
  where
    prop = buildNotCrashProp solution funcSig

    readResult :: BackendResult -> CandidateValidDesc
    readResult r@QC.GaveUp{QC.numTests}
              | numTests == 0 = Invalid
              | otherwise     = (Partial . pickExamples . parseExamples) r
    readResult r@QC.Success{} = (Total . pickExamples . parseExamples) r

-- >>> (evalStateT (classifyCandidate ["Data.Either", "GHC.List", "Data.Maybe", "Data.Function"] "\\e f -> Data.Either.either f (GHC.List.head []) e" (instantiateSignature $ parseTypeString "Either a b -> (a -> b) -> b") ["\\e f -> Data.Either.fromRight (f (Data.Maybe.fromJust Data.Maybe.Nothing)) e", "\\e f -> Data.Either.either f Data.Function.id e"]) emptyFilterState) :: IO CandidateDuplicateDesc
classifyCandidate :: MonadIO m => [String] -> Candidate -> FunctionSignature -> [Candidate] -> FilterTest m CandidateDuplicateDesc
classifyCandidate modules candidate funcSig previousCandidates = if null previousCandidates then return (New []) else do
    let properties    =   buildDupCheckProp (candidate, previousCandidates) funcSig
    interpreterResult <-  evaluateProperties modules funcSig properties

    let examples      =   readInterpreterResult candidate previousCandidates interpreterResult

    if all isJust examples
      then return $ New         (mergeExamples $ map fromJust examples)
      else return $ DuplicateOf (fst $ head $ filter snd $ zip previousCandidates $ map isJust examples)
  where
    readResult :: Candidate -> Candidate -> BackendResult -> Maybe AssociativeInternalExamples
    readResult candidate previousCandidate result = case result of
        QC.Failure {}                           -> Nothing
        QC.GaveUp {QC.numTests} | numTests == 0 -> Nothing
        QC.GaveUp {}                            -> assocs
        QC.Success {}                           -> assocs
      where
        (examples, examplesForPrev) = splitConsecutive $ parseExamples result
        assocs = Just [(candidate, examples), (previousCandidate, examplesForPrev)]

    readInterpreterResult :: Candidate -> [Candidate] -> Either InterpreterError [BackendResult] -> [Maybe AssociativeInternalExamples]
    readInterpreterResult candidate previousCandidates =
      \case
        Left (NotAllowed _) -> [Nothing]
        Left err            -> []
        Right results       -> zipWith (readResult candidate) previousCandidates results

    mergeExamples :: [AssociativeInternalExamples] -> AssociativeInternalExamples
    mergeExamples rawExamples =
      let exampleMaps = map (Map.fromList . map (second (take 2))) rawExamples in
        Map.toList $ Map.unionsWith (++) exampleMaps

runChecks :: MonadIO m => Environment -> TypeSkeleton -> UProgram -> FilterTest m (Maybe AssociativeExamples)
runChecks env goalType prog = do
    let (modules, funcSigStr, candidate, _) =   extractSolution env goalType prog
    let funcSig                             =   instantiateSignature $ parseTypeString funcSigStr
    result                                  <-  andM $ map (\f -> f modules funcSig candidate) checks

    if result
      then get    >>=   \s -> liftIO $ printFilter candidate s
      else modify $     \s -> s {discardedSolutions = candidate : discardedSolutions s}

    if result
      then get <&> (Just . map (second (map toExample)) . Map.toList . differentiateExamples)
      else return Nothing
  where
    checks = [ checkSolutionNotCrash, checkDuplicates]

checkSolutionNotCrash :: MonadIO m => [String] -> FunctionSignature -> String -> FilterTest m Bool
checkSolutionNotCrash modules funcSig solution = do
  result <- validateCandidate modules solution funcSig
  modify $ \s -> s {solutionDescriptions = (solution, result) : solutionDescriptions s }
  return $ isSuccess result

checkDuplicates :: MonadIO m => [String] -> FunctionSignature -> String -> FilterTest m Bool
checkDuplicates modules funcSig candidate = do
  FilterState previousCandidates _ _ _ _ <- get
  result <- classifyCandidate modules candidate funcSig previousCandidates

  case result of
    DuplicateOf _ -> return False
    New assocExamples -> do
      modify $ \s -> s {
        solutions             = candidate : solutions s,
        differentiateExamples = Map.unionWith (++) (differentiateExamples s) (Map.fromList assocExamples)
      }

      return True

-- | Format the placeholder for parameters to some Haskell representation.
-- (plain variables, typed variables, list of show, unwrapped variables)
-- >>> showParams [Concrete "Int", Concrete "String"]
-- ("(arg_1) (arg_2)","(arg_1 :: MyInt) (arg_2 :: [MyChar])","[(show arg_1), (show arg_2)]","(unwrap arg_1) (unwrap arg_2)")
showParams :: [ArgumentType] -> (String, String, String, String)
showParams args = (plain, typed, shows, unwrp)
  where
    args' = zip [1..] $ map replaceMyType args

    plain = unwords $ formatIdx "(arg_%d)"
    unwrp = unwords $ formatIdx "(unwrap arg_%d)"
    typed = unwords $ map (\(idx, tipe) -> printf "(arg_%d :: %s)" idx (show tipe) :: String) args'

    shows = "[" ++ intercalate ", " (formatIdx "(show arg_%d)") ++ "]"
    formatIdx format = map ((printf format :: Int -> String) . fst) args'

-- | Parse Example string from QC.label to Example
parseExamples :: BackendResult -> [InternalExample]
parseExamples result = concatMap (read . head) $ Map.keys $ QC.labels result

-- | Extract higher-order arguments from a type signature.
-- >>> extractHigherOrderQuery $ parseTypeString "a -> (a -> b) -> [(a -> b -> c)] -> b"
-- [((a) -> (b)),((a) -> (((b) -> (c))))]
extractHigherOrderQuery :: FunctionSignature -> [ArgumentType]
extractHigherOrderQuery FunctionSignature{_constraints, _argsType, _returnType} = nub $ concatMap (`extract` []) _argsType
  where
    extract :: ArgumentType -> [ArgumentType] -> [ArgumentType]
    extract argType xs = case argType of
      ArgTypeFunc   l r   -> argType : xs
      ArgTypeTuple  types -> concatMap (`extract` []) types ++ xs
      ArgTypeList   sub   -> extract sub [] ++ xs
      ArgTypeApp    l r   -> extract l [] ++ extract r [] ++ xs
      _                   -> xs

-- | Given type queries, query Hoogle for components.
-- >>> queryHoogle ["a -> a", "a -> a -> a"]
-- [["id"],["asTypeOf","const","seq"]]
queryHoogle :: [String] -> IO [[String]]
queryHoogle types = Hoogle.defaultDatabaseLocation >>= (`Hoogle.withDatabase` invokeQuery)
  where invokeQuery db = do
              let functions = map ( take 5 .
                                  nubOrd .
                                  map ( head .
                                        splitOn " :: " .
                                        unHTML .
                                        Hoogle.targetItem
                                      ) .
                                  filter isInModuleList .
                                  filter doesNotHaveTypeClass .
                                  Hoogle.searchDatabase db .
                                  (++) ":: "
                                ) types
              return functions
        isInModuleList x = let Just (name, _) = Hoogle.targetModule x in name `elem` hoogleQueryModuleList
        doesNotHaveTypeClass Hoogle.Target{Hoogle.targetItem} = not ("=&gt;" `isInfixOf` targetItem)

queryHooglePlus :: [String] -> IO [[String]]
queryHooglePlus types = return []

queryHigherOrderArgument :: MonadIO m => [String] -> FilterTest m [(String, [String])]
queryHigherOrderArgument queries = do
    cache             <-  gets higherOrderArgumentCache
    let cachedResults =   zipWith (curry $ second $ fromMaybe []) queries (map (`Map.lookup` cache) queries)

    let nextQueries   =   map fst $ filter (null . snd) cachedResults
    queryResults      <-  Map.unionsWith (++) <$> mapM (\f -> Map.fromList . zip nextQueries <$> liftIO (f nextQueries)) searchFunctions

    let result        =   Map.union (Map.fromList cachedResults) queryResults

    modify (\state -> state { higherOrderArgumentCache = Map.union (higherOrderArgumentCache state) result })
    return (Map.toList result)
  where
    searchFunctions = [queryHoogle, queryHooglePlus]


-- >>> prepareEnvironment $ parseTypeString "a -> (Int -> Int -> Int) -> a"
-- >>> runInterpreterWithEnvTimeoutHOF (10^8) (parseTypeString "a -> (Int -> [Int]) -> a") (return "114514")
prepareEnvironment :: MonadIO m => FunctionSignature -> FilterTest m String
prepareEnvironment funcSig = do
    let higherOrderTypes = extractHigherOrderQuery funcSig
    let higherOrderTypeStrs = map show higherOrderTypes

    -- order of results is not preserved. Here, we reorder it to pair it with higher-ordered types.
    -- Why we want to pair it? Because type signature contains ``Box'' information.
    queryResults <- queryHigherOrderArgument higherOrderTypeStrs <&> \result -> map (fromMaybe [] . (`lookup` result)) higherOrderTypeStrs

    tmpDir <- liftIO getTmpDir
    baseName <- liftIO nextRandom
    let baseNameStr = show baseName ++ ".hs"
    let fileName = tmpDir ++ "/" ++ baseNameStr

    let sourceCode = buildEnvFileContent higherOrderTypes queryResults
    if all null queryResults
      then liftIO $ writeFile fileName ""
      else liftIO $ writeFile fileName sourceCode

    return fileName
  where
    prelude = [ "{-# LANGUAGE FlexibleInstances #-}"
              , "module HigherOrderParamsEnv where"
              , "import Control.Monad"
              , "import Test.QuickCheck"
              , "import InternalTypeGen"
              ] ++ map ("import " ++) hoogleQueryModuleList

    postlude = "instance Arbitrary (%s) where arbitrary = sized $ \\n -> if n < %d then elements insFunc_%d else liftM Generated arbitrary"
    buildInstanceFunction tipe expr = printf "(Expression %s (%s))" (show expr) (buildExpression tipe expr) :: String
    buildInstanceFunctions tipe exprs = "[" ++ intercalate ", " (map (buildInstanceFunction tipe) exprs) ++ "]"

    buildEnvFileContent tipes exprGroups = unlines $ prelude ++ concat (zipWith3 buildStep [(1 :: Int)..] tipes exprGroups)
      where
        buildStep _ tipe []    = ["-- Bypass building " ++ show tipe]
        buildStep i tipe exprs =
          let functionType = (buildAppType $ replaceMyType tipe) in
            [ printf "insFunc_%d :: [%s]" i functionType
            , printf "insFunc_%d = %s" i (buildInstanceFunctions tipe exprs)
            , printf postlude functionType higherOrderGenMaxSize i
            ] :: [String]

    buildAppType :: ArgumentType -> String
    buildAppType = \case
            ArgTypeFunc l r   -> printf "MyFun (%s) (%s)" (buildAppType l) (buildAppType r)
            otherType         -> show otherType

    buildExpression :: ArgumentType -> String -> String
    buildExpression tipe expr = printf "wrap ((%s) :: %s)" expr (show tipe)
