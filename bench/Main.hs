{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveAnyClass #-}
import System.Process (readCreateProcessWithExitCode, shell)
import System.IO
import Text.Printf (printf)
import System.Directory (removePathForcibly, createDirectory)
import Data.Aeson
import Data.ByteString.Lazy.Char8 (unpack)
import GHC.Generics

-- the examples are written in the same format of bench of the orginal tygar
data Example = Example {
    inputs :: [String],
    output :: String
} deriving(Eq, Generic)

instance ToJSON Example
instance FromJSON Example

logsDir :: String
logsDir = "bench-logs-ext"

resultsFile :: String
resultsFile = "results.log"

count :: Int
count = 90

exercises :: [(String, String, Maybe [Example])]
exercises = [("mapAdd1", "[Int] -> [Int]", Just [Example {inputs = ["[1, 2, 3]"], output = "[1, 2, 3]"}])]

execExercise :: (String, String, Maybe [Example]) -> IO (String, Maybe (Double, Double)) -- match, total time of first solution
execExercise (name, ty, mexs) = do
  -- run exercise
  let log = logsDir ++ "/" ++ name ++ ".log"
  readCreateProcessWithExitCode (shell $ command ty mexs log) ""
  hPutStrLn stderr $ command ty mexs log
  hFlush stderr
  -- read results
  file <- readFile log
  let ls = lines file
  if length ls > 1
    then do
      let ws = words (ls !! 1) 
      return $ (name, Just (read $ ws !! 0, read $ ws !! 1))
    else return (name, Nothing)
  where
    command :: String -> Maybe [Example] -> String -> String
    command ty Nothing log = printf "timeout 60s stack exec -- hplus \"%s\" --example=\"[]\" --cnt=%d --out=%s" ty count log
    command ty (Just exs) log = printf "timeout 60s stack exec -- hplus \"%s\" --example=\"%s\" --cnt=%d --out=%s" ty (examplesStr exs) count log 

printStats :: Handle -> (String, Maybe (Double, Double)) -> IO ()
printStats h (name, Nothing) = hPutStrLn h $ name ++ ": no solution in time."
printStats h (name, Just (mt, tt)) = hPutStrLn h $ name ++ ": " ++ show tt ++ " seconds. Match: " ++ show mt ++ " seconds."

examplesStr :: [Example] -> String
examplesStr [] = "[]" 
examplesStr (h:t) = "[" ++ exampleStr h ++ foldr (\c r -> ", " ++ exampleStr c ++ r) "]" t
  where
    exampleStr :: Example -> String
    exampleStr Example{inputs = is, output = o} = 
      "([" ++ inputsStr is ++ "], " ++ o ++ ")"
      where
        inputsStr :: [String] -> String
        inputsStr [] = ""
        inputsStr (h:t) = h ++ foldr (\c r -> ", " ++ c ++ r) "" t

main :: IO ()
main = do
  removePathForcibly logsDir
  createDirectory logsDir
  readCreateProcessWithExitCode (shell "stack exec -- hplus generate --preset=partialfunctions") ""
  stats <- mapM execExercise exercises
  mapM_ (printStats stdout) stats
  withFile (logsDir ++ "/" ++ resultsFile) WriteMode $ \handle -> do
    mapM_ (printStats handle) stats