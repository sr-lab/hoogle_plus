import System.Process (readCreateProcessWithExitCode, shell)
import System.IO
import Text.Printf (printf)
import System.Directory (removePathForcibly, createDirectory)
import Types.IOFormat
import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 (unpack)

logsDir :: String
logsDir = "bench-logs-examples"

resultsFile :: String
resultsFile = "results.log"

count :: Int
count = 5

exercises :: [(String, String, [Example])]
exercises = [
  ("mapAdd", "[Int] -> [Int]", [Example {inputs = ["[1, 2, 3]"], output = "[2, 3, 4]"}]),
  ("mapSquare", "[Int] -> [Int]", [Example {inputs = ["[1, 2, 3]"], output = "[1, 4, 9]"}]),
  ("appendConst", "[Int] -> [Int]", [Example {inputs = ["[1, 2, 3]"], output = "[1, 2, 3, 4]"}]),
  ("filterDiff", "[Int] -> [Int]", [Example {inputs = ["[1, 2, 3]"], output = "[1, 3]"}]),
  ("getFirstOnes", "[Int] -> [Int]", [Example {inputs = ["[1, 1, 0, 1, 2]"], output = "[1, 1]"}]),
  
  -- stackoverflow
  ("removeFirstOnes", "[Int] -> [Int]", [Example {inputs = ["[1, 1, 0, 1, 2]"], output = "[0, 1, 2]"}]),
  ("listIntersect", "[Int] -> [Int] -> [Int]", [Example {inputs = ["[0, 2, 4]", "[2, 4, 6"], output = "[2, 4]"}]),
  ("indexConst", "[Int] -> Int", [Example {inputs = ["[1, 2, 3]"], output = "3"}]),
  ("allGreaterThan", "[Int] -> Bool", [Example {inputs = ["[2, 3, 4]"], output = "Data.Bool.True"}, 
                              Example {inputs = ["[2, 1, 4]"], output = "Data.Bool.False"}]),
  ("dropConst", "[Int] -> [Int]", [Example {inputs = ["[1, 2, 3, 4, 5]"], output = "[3, 4, 5]"}]),
  ("filterGreaterThan", "[Int] -> [Int]", [Example {inputs = ["[2, 0, 1, 3]"], output = "[2, 3]"}]),
  ("filterPairs", "[(Int, Int)] -> [(Int, Int)]", [Example {inputs = ["[(1, 2), (3, 3), (4, 5)]"], output = "[(3, 3)"}]),
  ("filterEq", "[Int] -> [Int]", [Example {inputs = ["[1, 2, 1, 3, 4, 4]"], output = "[1, 1]"}]),
  ("replicateConst", "[Int]", [Example {inputs = [], output = "[3, 3, 3, 3, 3]"}]),
  ("addElemsTwoLists", "[Int] -> [Int] -> [Int]", [Example {inputs = ["[1, 2, 3]", "[3, 4, 5]"], output = "[4, 6, 8]"}])
  ]

-- example with 
--("tail", "[Int] -> [Int]", [Example {inputs = ["[1, 2, 3]"], output = "[2, 3]"}])

execExercise :: (String, String, [Example]) -> IO (String, Maybe Double) -- total time of first solution
execExercise (name, ty, mexs) = do
  -- run exercise
  let log = logsDir ++ "/" ++ name ++ ".log"
  hPutStrLn stderr $ command ty mexs log
  hFlush stderr
  readCreateProcessWithExitCode (shell $ command ty mexs log) ""
  -- read results
  file <- readFile log
  let ls = lines file
  if length ls > 1
    then return $ (name, Just (read (ls !! 1) :: Double))
    else return (name, Nothing)
  where
    command :: String -> [Example] -> String -> String
    command ty exs log = printf "timeout 60s stack exec -- hplus --json=\'{\"query\":\"%s\", \"inExamples\":%s}\' --cnt=%d --out=%s" ty (unpack $ encode exs) count log 

printStats :: Handle -> (String, Maybe Double) -> IO ()
printStats h (name, Nothing) = hPutStrLn h $ name ++ ": no solution in time."
printStats h (name, Just tm) = hPutStrLn h $ name ++ ": " ++ show tm ++ " seconds."

main :: IO ()
main = do
  readCreateProcessWithExitCode (shell "stack exec -- hplus generate --preset=partialfunctions") ""
  removePathForcibly logsDir
  createDirectory logsDir
  stats <- mapM execExercise exercises
  mapM_ (printStats stdout) stats
  withFile (logsDir ++ "/" ++ resultsFile) WriteMode $ \handle -> do
    mapM_ (printStats handle) stats