import System.Process (readCreateProcessWithExitCode, shell)
import System.IO
import Text.Printf (printf)
import System.Directory (removePathForcibly, createDirectory)
import Types.IOFormat
import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 (unpack)

logsDir :: String
logsDir = "bench-logs-orig"

resultsFile :: String
resultsFile = "results.log"

count :: Int
count = 5

exercises :: [(String, String, Maybe [Example])]
exercises = [("tail", "[Int] -> [Int]", Just [Example {inputs = ["[1, 2, 3]"], output = "[2, 3]"}])]

execExercise :: (String, String, Maybe [Example]) -> IO (String, Maybe Double) -- total time of first solution
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
    then return $ (name, Just (read (ls !! 1) :: Double))
    else return (name, Nothing)
  where
    command :: String -> Maybe [Example] -> String -> String
    command ty Nothing log = printf "timeout 60s stack exec -- hplus --json=\'{\"query\":\"%s\", \"inExamples\":[]}\' --cnt=%d --out=%s" ty count log
    command ty (Just exs) log = printf "timeout 60s stack exec -- hplus --json=\'{\"query\":\"%s\", \"inExamples\":%s}\' --cnt=%d --out=%s" ty (unpack $ encode exs) count log 

printStats :: Handle -> (String, Maybe Double) -> IO ()
printStats h (name, Nothing) = hPutStrLn h $ name ++ ": no solution in time."
printStats h (name, Just tm) = hPutStrLn h $ name ++ ": " ++ show tm ++ " seconds."

main :: IO ()
main = do
  removePathForcibly logsDir
  createDirectory logsDir
  stats <- mapM execExercise exercises
  mapM_ (printStats stdout) stats
  withFile (logsDir ++ "/" ++ resultsFile) WriteMode $ \handle -> do
    mapM_ (printStats handle) stats