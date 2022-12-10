{-# LANGUAGE ScopedTypeVariables #-}
module Database.Environment(
    writeEnv
  , generateEnv
  , toFunType
  , getFiles
  , filesToEntries
  , filesToLinearSynthSymbs
  ) where

import Data.Either
import Data.Serialize (encode)
import Data.List.Extra
import Control.Lens ((^.))
import qualified Data.ByteString as B
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State (evalStateT)
import System.Exit (exitFailure)
import Text.Parsec.Pos (initialPos)
import Text.Printf
import Data.Maybe (fromJust)

import Synquid.Error (Pos(Pos))
import Synquid.Logic (ftrue)
import Types.Type -- (BaseType(..), TypeSkeleton(..), SchemaSkeleton(..))
import Synquid.Type (isHigherOrder, toMonotype)
import Synquid.Pretty as Pretty
import Database.Util
import qualified Database.Download as DD
import qualified Database.Convert as DC
import Types.Environment
import Types.Program (BareDeclaration(..), Declaration(..), ConstructorSig(..))
import Types.Generate
import Synquid.Resolver (resolveDecls)
import qualified Data.List.Utils as LUtils
import qualified Types.Program as TP
import Synquid.Util
import qualified Debug.Trace as D

import qualified HooglePlus.Example as Example
import qualified SymbolicMatch.Samples as S

import Debug.Trace (trace)
import Data.Typeable (typeOf)
import Control.Monad.IO.Class (liftIO)
import Types.Filtering (FunctionSignature)
import qualified Data.Text as T
import HooglePlus.FilterTest (parseTypeString)
import System.IO

writeEnv :: FilePath -> Environment -> IO ()
writeEnv path env = B.writeFile path (encode env)

-- getDeps will try its best to come up with the declarations needed to satisfy unmet type dependencies in ourEntries.
-- There are the entries in the current set of packages (allEntries), and the strategy to look at other packages.
getDeps :: PackageFetchOpts -> Map MdlName [Entry] -> [Entry] -> IO [Declaration]
getDeps Local{files=f} allEntries ourEntries = do
  let dependentEntries = DC.entryDependencies allEntries ourEntries (concat $ Map.elems allEntries)
  nubOrd <$> mapM (flip evalStateT 0 . DC.toSynquidDecl) dependentEntries
getDeps Hackage{packages=ps} allEntries ourEntries = do
  pkgsDeps <- mapM (\pkgName -> do
    pkgDeps <- nubOrd <$> DC.packageDependencies pkgName True
    entriesFromDeps <- concatMap (concat . Map.elems) <$> (mapM (flip DC.readDeclarations Nothing) pkgDeps)
    let dependentEntries = DC.entryDependencies allEntries ourEntries entriesFromDeps
    mapM (flip evalStateT 0 . DC.toSynquidDecl) dependentEntries
    ) ps
  return $ nubOrd $ concat pkgsDeps

generateEnv :: GenerationOpts -> IO Environment
generateEnv genOpts = do
    let useHO = enableHOF genOpts
    let pkgOpts = pkgFetchOpts genOpts
    let mdls = modules genOpts
    let pathToHo = hoPath genOpts
    let mbModuleNames = if length mdls > 0 then Just ("Symbol":mdls) else Nothing
    pkgFiles <- getFiles pkgOpts
    allEntriesByMdl <- filesToEntries pkgFiles True
    symsToLS <- filesToLinearSynthSymbs pkgFiles mdls
    DD.cleanTmpFiles pkgOpts pkgFiles
    let entriesByMdl = filterEntries allEntriesByMdl mbModuleNames
    let ourEntries = nubOrd $ concat $ Map.elems entriesByMdl
    dependencyEntries <- getDeps pkgOpts allEntriesByMdl ourEntries
    let moduleNames = Map.keys entriesByMdl
    let allCompleteEntries = concat (Map.elems entriesByMdl)
    let allEntries = nubOrd allCompleteEntries
    ourDecls <- mapM (\(entry) -> (evalStateT (DC.toSynquidDecl entry) 0)) allEntries
    let instanceDecls = filter (\entry -> DC.isInstance entry) allEntries
    let instanceRules = map DC.getInstanceRule instanceDecls
    let transitionIds = [0 .. length instanceRules]
    let instanceTuples = zip instanceRules transitionIds
    instanceFunctions <- mapM (\(entry, id) -> evalStateT (DC.instanceToFunction entry id) 0) instanceTuples

    -- TODO: remove all higher kinded type instances
    let instanceFunctions' = filter (\x -> not(or [(isInfixOf "Applicative" $ show x),(isInfixOf "Functor" $ show x),(isInfixOf "Monad" $ show x)])) instanceFunctions

    let declStrs = show (instanceFunctions' ++ ourDecls)
    let removeParentheses = (\x -> LUtils.replace ")" "" $ LUtils.replace "(" "" x)
    let tcNames = nub $ map removeParentheses $ filter (\x -> isInfixOf tyclassPrefix x) (splitOn " " declStrs)
    let tcDecls = map (\x -> Pos (initialPos "") $ TP.DataDecl x ["a"] [] []) tcNames
    let library = concat [ourDecls, dependencyEntries, instanceFunctions', tcDecls, defaultLibrary]
    let hooglePlusDecls = DC.reorderDecls $ nubOrd $ library

    result <- case resolveDecls hooglePlusDecls moduleNames of
       Left errMessage -> error $ show errMessage
       Right env -> do
            let env' = env { _symbols = if useHO then env ^. symbols
                                                else Map.filter (not . isHigherOrder . toMonotype) $ env ^. symbols,
                           _included_modules = Set.fromList (moduleNames)
                          }
            hofStr <- readFile pathToHo
            let hofNames = words hofStr
            -- get signatures
            let sigs = map (\f -> lookupWithError "env: symbols" f (env' ^. symbols)) hofNames
            -- transform into fun types and add into the environments
            let sigs' = zipWith (\n t -> (n ++ hoPostfix, toFunType t)) hofNames sigs
            let env'' = env' { _symbols = Map.union (env' ^. symbols) (Map.fromList sigs')
                             , _hoCandidates = map fst sigs'
                             , _symsToLinearSynth = symsToLS
                             , _included_modules = Set.filter (/= "Symbol") $ _included_modules env' }
            return env''
    printStats result
    return result
   where
     filterEntries entries Nothing = entries
     filterEntries entries (Just mdls) = Map.filterWithKey (\m _-> m `elem` mdls) entries

toFunType :: RSchema -> RSchema
toFunType (ForallT x t) = ForallT x (toFunType t)
toFunType (Monotype (FunctionT x tArg tRes)) = let
  tArg' = toMonotype $ toFunType $ Monotype tArg
  tRes' = toMonotype $ toFunType $ Monotype tRes
  in Monotype $ ScalarT (DatatypeT "Fun" [tArg', tRes'] []) ftrue
toFunType t = t

-- filesToEntries reads each file into map of module -> declartions
-- Filters for modules we care about. If none, use them all.
filesToEntries :: [FilePath] -> Bool -> IO (Map MdlName [Entry])
filesToEntries fps renameFunc = do
    declsByModuleByFile <- mapM (\fp -> DC.readDeclarationsFromFile fp renameFunc) fps
    -- we add to the existing components symbols (symbolInt, symbolList...)
    -- that are going to be replaced by the match algorithm
    let symbolModule = DC.readDeclarationsFromStrings Example.symbolsDecls renameFunc
    return $ Map.unionsWith (++) (symbolModule:declsByModuleByFile)

filesToLinearSynthSymbs :: [FilePath] -> [String] -> IO [(String, FunctionSignature, Int)]
filesToLinearSynthSymbs fps modules = foldr (\fp r -> do r' <- r; c <- readFileToLS fp; return $ c ++ r') (return []) fps
  where 
    modulesText :: [T.Text]
    modulesText = map T.pack modules

    readFileToLS :: FilePath -> IO [(String, FunctionSignature, Int)]
    readFileToLS fp = do 
      h <- openFile fp ReadMode
      hSetEncoding h utf8
      s <- hGetContents h
      return $ parseLines (T.lines $ T.pack s) (T.pack "") False []
      where 
        parseLines :: [T.Text] 
                   -> T.Text
                   -> Bool -- whether the current module is to consider or not
                   -> [(String, FunctionSignature, Int)]
                   -> [(String, FunctionSignature, Int)]
        parseLines [] _ modOk acc = acc
        parseLines (h:t) curMod modOk acc
          -- current line is a module declaration
          | modOk && T.pack "forall" `T.isInfixOf` h = trace ("forall not supported on type declarations, skip declaration:" ++ T.unpack h) $ parseLines t curMod modOk acc
          | Just mod <- T.stripPrefix (T.pack "module ") (T.strip h) =
            let str = T.strip mod in
              if T.null str 
                then error "parse file LS" 
                else parseLines t str (str `elem` modulesText) acc
          -- current line is to ignore; no definition os function nor module
          | T.pack "@" `T.isPrefixOf` (T.strip h) = parseLines t curMod modOk acc
          | T.pack "data " `T.isPrefixOf` (T.strip h) = parseLines t curMod modOk acc
          | T.pack "instance " `T.isPrefixOf` (T.strip h) = parseLines t curMod modOk acc
          | T.pack "class " `T.isPrefixOf` (T.strip h) = parseLines t curMod modOk acc
          | T.pack "infix " `T.isPrefixOf` (T.strip h) = parseLines t curMod modOk acc
          | T.pack "infixr " `T.isPrefixOf` (T.strip h) = parseLines t curMod modOk acc
          | T.pack "infixl " `T.isPrefixOf` (T.strip h) = parseLines t curMod modOk acc
          | T.pack "type " `T.isPrefixOf` (T.strip h) = parseLines t curMod modOk acc
          | T.pack "newtype " `T.isPrefixOf` (T.strip h) = parseLines t curMod modOk acc
          | T.pack "--" `T.isPrefixOf` (T.strip h) = parseLines t curMod modOk acc
          -- current line is a declaration id :: type, but ignore because 
          -- that modules is not to consider
          | not modOk = parseLines t curMod modOk acc
          -- current line is a declaration id :: type, and the modules is to consider
          | otherwise = let decl = T.breakOn (T.pack "::") h in
              let nm = T.unpack $ T.strip (fst decl) 
                  ty = T.unpack $ fromJust $ T.stripPrefix (T.pack "::") $ T.strip (snd decl) in
                if null nm || null ty 
                  then error $ printf "name or type empty in %s" h
                  else 
                    let nmMod = if head nm == '('
                        then '(' : ((T.unpack curMod) ++ "." ++ (tail nm)) 
                        else (T.unpack curMod) ++ "." ++ nm 
                      in case S.lookupFun nmMod of -- FIXME (2 things):trace should be error? reject h.o.
                        Nothing -> {-trace (printf "Function not in match: %s" (show nmMod)) $-} parseLines t curMod modOk acc
                        Just i -> parseLines t curMod modOk ((nmMod, parseTypeString ty , i):acc)

getFiles :: PackageFetchOpts -> IO [FilePath]
getFiles Hackage{packages=p} = mapM DD.getPkg p >>= (return . concat)
getFiles Local{files=f} = return f

printStats :: Environment -> IO ()
printStats env = do
  let typeMap = env ^. datatypes
  let modules = _included_modules env
  let typeclassInstances = _typClassInstances env
  let symbols = _symbols env
  let symbolsCount = Map.size symbols
  let typeCount = Map.size typeMap
  printf "types: %d; symbols: %d\n" typeCount symbolsCount
  printf "included types: %s\n" $ show (Map.keys typeMap)
  printf "included modules: %s\n" $ show (Set.elems modules)