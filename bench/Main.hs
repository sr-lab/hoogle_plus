{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, NamedFieldPuns #-}

module Main (main) where

import Synquid.Logic
import Synquid.Type
import Synquid.Program
import Synquid.Error
import Synquid.Pretty
import Synquid.Parser (parseFromFile, parseProgram, toErrorMessage)
import Synquid.Resolver (resolveDecls, ResolverState (..), initResolverState, resolveSchema)
import Synquid.SolverMonad
import Types.Generate hiding (files)
import Types.Experiments
import Types.Environment
import Types.Program
import Types.Solver
import Synquid.HtmlOutput
import Database.Presets
import Database.Environment
import Database.Convert
import Database.Generate
import Database.Download
import Database.Util
import Synquid.Util (showme)
import HooglePlus.Synthesize
import HooglePlus.Stats
import Types.Encoder
import HooglePlus.GHCChecker
import HooglePlus.Utils
import HooglePlus.Example

import Control.Monad
import Control.Lens ((^.))
import System.Exit
import System.Console.CmdArgs hiding (Normal)
import System.Console.ANSI
import System.FilePath
import Text.Parsec.Pos
import Text.Printf
import Text.Pretty.Simple
import Control.Monad.State (runState, evalStateT, execStateT, evalState)
import Control.Monad.Except (runExcept)
import Control.Concurrent
import Control.Concurrent.Chan
import Data.Char
import Data.List
import Data.Foldable
import Data.Serialize
import Data.Time.Calendar
import Data.Maybe (isJust)
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Language.Haskell.Exts (Decl(TypeSig))
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (mapMaybe, fromJust)
import Distribution.PackDeps
import Text.Parsec hiding (State)
import Text.Parsec.Indent
import System.Directory
import System.IO
import qualified Data.ByteString as B

import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Text.PrettyPrint.ANSI.Leijen (fill, column)

import Data.List.Split

programName = "hoogleplus"
versionName = "0.1"
releaseDate = fromGregorian 2019 3 10

-- | Type-check and synthesize a program, according to command-line arguments
main = do
  let searchParams =
          defaultSearchParams
              { _maxApplicationDepth = app_max
              , _explorerLogLevel = log_
              , _solutionCnt = sol_num
              , _useHO = not disable_higher_order
              , _stopRefine = stop_refine
              , _threshold = stop_threshold
              , _incrementalSolving = incremental
              , _refineStrategy = use_refine
              , _disableDemand = disable_demand
              , _coalesceTypes = not disable_coalescing
              , _coalesceStrategy = coalescing_strategy
              , _disableRelevancy = disable_relevancy
              , _disableCopy = disable_copy_trans
              , _disableBlack = disable_blacklist
              , _disableFilter = disable_filter
              }
  let synquidParams =
          defaultSynquidParams {Main.envPath = env_file_path_in}
  if exampleStr == "" 
    then executeSearch synquidParams searchParams file Nothing
    else case parseExamples exampleStr of
      Left err -> putStrLn err
      Right examples -> executeSearch synquidParams searchParams file (Just examples)
        Generate {preset = (Just preset)} -> do
            precomputeGraph (getOptsFromPreset preset)
        Generate Nothing files pkgs mdls d ho pathToEnv hoPath -> do
            let fetchOpts =
                    if (length files > 0)
                        then Local files
                        else defaultHackageOpts {packages = pkgs}
            let generationOpts =
                    defaultGenerationOpts
                        { modules = mdls
                        , instantiationDepth = d
                        , enableHOF = ho
                        , pkgFetchOpts = fetchOpts
                        , Types.Generate.envPath = pathToEnv
                        , Types.Generate.hoPath = hoPath
                        }
            precomputeGraph generationOpts


{- Command line arguments -}

{-# ANN module "HLint: ignore Use camelCase" #-}
{-# ANN module "HLint: ignore Redundant bracket" #-}
{-# ANN module "HLint: ignore" #-}


mode = cmdArgsMode $ modes [synt, generate] &=
  help (programName ++ " program synthesizer") &=
  program programName &=
  summary (programName ++ " v" ++ versionName ++ ", " ++ showGregorian releaseDate)

-- | Output format
data OutputFormat = Plain -- ^ Plain text
  | Ansi -- ^ Text with ANSI-terminal special characters
  | Html -- ^ HTML
  deriving (Typeable, Data, Eq, Show)

-- | 'printDoc' @format doc@ : print @doc@ to the console using @format@
printDoc :: OutputFormat -> Doc -> IO()
printDoc Plain doc = putDoc (plain doc) >> putStr "\n"
printDoc Ansi doc = putDoc doc >> putStr "\n"
printDoc Html doc = putStr (showDocHtml (renderPretty 0.4 100 doc))

-- | Parameters of the synthesis
data SynquidParams = SynquidParams {
    envPath :: String -- ^ Path to the environment file
}

defaultSynquidParams = SynquidParams {
    Main.envPath = defaultEnvPath
}

precomputeGraph :: GenerationOpts -> IO ()
precomputeGraph opts = generateEnv opts >>= writeEnv (Types.Generate.envPath opts)


-- | Parse and resolve file, then synthesize the specified goals
executeSearch :: SynquidParams -> SearchParams  -> String -> Maybe [Example] -> IO ()
executeSearch synquidParams searchParams query examples = do
  env <- readEnv
  goal <- envToGoal env query
  solverChan <- newChan
  checkerChan <- newChan
  workerS <- forkIO $ synthesize searchParams goal solverChan (isJust examples)
  workerC <- forkIO $ check goal searchParams solverChan checkerChan examples
  readChan checkerChan >>= (handleMessages checkerChan)
  where
    logLevel = searchParams ^. explorerLogLevel
    readEnv = do
      let envPathIn = Main.envPath synquidParams
      doesExist <- doesFileExist envPathIn
      when (not doesExist) (error ("Please run `stack exec -- " ++ programName ++ " generate -p [PACKAGES]` to generate database first"))
      envRes <- decode <$> B.readFile envPathIn
      case envRes of
        Left err -> error err
        Right env ->
          return env

    handleMessages ch (MesgClose _) = when (logLevel > 0) (putStrLn "Search complete") >> return ()
    handleMessages ch (MesgP (program, stats, _)) = do
      when (logLevel > 0) $ printf "[writeStats]: %s\n" (show stats)
      printSolution program
      printf "[writeStats]: %s\n" (show stats)
      hFlush stdout
      readChan ch >>= (handleMessages ch)
    handleMessages ch (MesgS debug) = do
      when (logLevel > 1) $ printf "[writeStats]: %s\n" (show debug)
      readChan ch >>= (handleMessages ch)
    handleMessages ch (MesgLog level tag msg) = do
      when (level <= logLevel) (do
        mapM (printf "[%s]: %s\n" tag) (lines msg)
        hFlush stdout)
      readChan ch >>= (handleMessages ch)

pdoc = printDoc Plain
