{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveAnyClass #-}
import System.Process
import System.IO
import Text.Printf (printf)
import System.Directory (removePathForcibly, createDirectory)
import Data.Aeson
import Data.ByteString.Lazy.Char8 (unpack)
import GHC.Generics
import Control.Monad
import System.Exit 
import GHC.Conc

-- the examples are written in the same format of bench of the orginal tygar
data Example = Example {
    inputs :: [String],
    output :: String
} deriving(Eq, Generic)

instance ToJSON Example
instance FromJSON Example

logsDir :: String
logsDir = "bench-logs-extension"

resultsFile :: String
resultsFile = "results.log"

count :: Int
count = 100

exercises :: [(String, String, [Example])]
exercises = [
  {-("firstRight", "[Either a b] -> Either a b", []),
  ("firstKey", "[(a, b)] -> a", []),
  ("flatten", "[[[a]]] -> [a]", []),
  ("repl-funcs", "(a -> b) -> Int -> [a -> b]", []),
  ("containsEdge", "Int -> (Int, Int) -> Bool", []),
  ("multiApp", "(a -> b -> c) -> (a -> b) -> a -> c", []),
  ("appendN", "Int -> [a] -> [a]", []),
  ("pipe", "[a -> a] -> (a -> a)", []),
  ("intToBS", "Int64 -> ByteString", []),
  ("cartProduct", "[a] -> [b] -> [[(a, b)]]", []),
  ("applyNtimes", "(a -> a) -> a -> Int -> a", []),
  ("firstMatch", "[a] -> (a -> Bool) -> a", []),
  ("mbElem", "Eq a => a -> [a] -> Maybe a", []),
  ("mapEither", "(a -> Either b c) -> [a] -> ([b], [c])", []),
  ("hoogle01", "(a -> b) -> [a] -> b", []),
  ("zipWithResult", "(a -> b) -> [a] -> [(a, b)]", []),
  ("splitStr", "String -> Char -> String", []),
  ("lookup", "[(a, b)] -> a -> b", []),
  ("fromFirstMaybes", "a -> [Maybe a] -> a", []),
  ("map", "(a -> b) -> [a] -> [b]", []),
  ("maybe", "Maybe a -> a -> Maybe a", []),
  ("rights", "[Either a b] -> Either a [b] ", []),
  ("mbAppFirst", "b -> (a -> b) -> [a] -> b", []),
  ("mergeEither", "Either a (Either a b) -> Either a b", []),
  ("test", "Bool -> a -> Maybe a", []),
  ("multiAppPair", "(a -> b, a -> c) -> a -> (b, c)", []),
  ("splitAtFirst", "a -> [a] -> ([a], [a])", []),
  ("2partApp", "(a->b)->(b->c)->[a]->[c]", []),
  ("areEq", "Eq a => a -> a -> Maybe a", []),
  ("eitherTriple", "Either a b -> Either a b -> Either a b", []),
  ("mapMaybes", "(a -> Maybe b) -> [a] -> Maybe b", []),
  ("head-rest", "[a] -> (a, [a])", []),
  ("appBoth", "(a -> b) -> (a -> c) -> a -> (b, c)", []),
  ("applyPair", "(a -> b, a) -> b", []),
  ("resolveEither", "Either a b -> (a->b) -> b", []),
  ("head-tail", "[a] -> (a,a)", []),
  ("indexesOf", "([(a,Int)] -> [(a,Int)]) -> [a] -> [Int] -> [Int]", []),
  ("app3", "(a -> b -> c -> d) -> a -> c -> b -> d", []),
  ("both", "(a -> b) -> (a, a) -> (b, b)", []),
  ("takeNdropM", "nt -> Int -> [a] -> ([a], [a])", []),
  ("firstMaybe", "[Maybe a] -> a", []),
  ("mbToEither", "Maybe a -> b -> Either a b", []),
  ("pred-match", "[a] -> (a -> Bool) -> Int", []),
  ("singleList", "Int -> [Int]", []),-}
  -- new exercises
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
  ("dropConst", "[Int] -> [Int]", [Example {inputs = ["[0, 4, 4, 3]"], output = "[4, 3]"}]),
  ("filterGreaterThan", "[Int] -> [Int]", [Example {inputs = ["[2, 0, 1, 3]"], output = "[2, 3]"}]),
  ("filterPairs", "[(Int, Int)] -> [(Int, Int)]", [Example {inputs = ["[(1, 2), (3, 3), (4, 5)]"], output = "[(3, 3)"}]),
  ("filterEq", "[Int] -> [Int]", [Example {inputs = ["[1, 2, 1, 3, 4, 4]"], output = "[1, 1]"}]),
  ("replicateConst", "[Int]", [Example {inputs = [], output = "[3, 3, 3, 3, 3]"}]),
  ("addElemsTwoLists", "[Int] -> [Int] -> [Int]", [Example {inputs = ["[1, 2, 3]", "[3, 4, 5]"], output = "[4, 6, 8]"}])
  ]

execExercisesPar :: Int -> [(String, String, [Example])] -> IO ()
execExercisesPar _ [] = return ()
execExercisesPar cores exs = do
  let execNow = take cores exs
  let execAfter = drop cores exs
  hs <- mapM execExercise execNow
  mapM_ waitForProcess hs
  execExercisesPar cores execAfter
  
execExercise :: (String, String, [Example]) -> IO ProcessHandle
execExercise (name, ty, exs) = do
  -- run exercise
  let log = logsDir ++ "/" ++ name ++ ".log"
  hPutStrLn stderr $ command ty exs log
  hFlush stderr
  (_, _, _, handle) <- createProcess $ (shell (command ty exs log)) --{std_err = CreatePipe, std_out = CreatePipe}
  return handle
  where
    command :: String -> [Example] -> String -> String
    command ty exs log = printf "timeout 60s stack exec -- hplus \"%s\" --example=\"%s\" --cnt=%d --out=%s 1> /dev/null 2> /dev/null" ty (examplesStr exs) count log 

readLogStats :: String -> IO (Maybe (Double, Double))
readLogStats name = do
  let logFilePath = logsDir ++ "/" ++ name ++ ".log"
  file <- readFile logFilePath
  let ls = lines file
  if length ls > 1
    then do
      let ws = words (ls !! 1) 
      return $ Just (read $ ws !! 0, read $ ws !! 1)
    else return Nothing

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
  --cores <- getNumProcessors
  let cores = 4
  execExercisesPar cores exercises
  stats <- mapM (\(n, _, _) -> do sts <- readLogStats n; return (n, sts)) exercises
  mapM_ (printStats stdout) stats
  withFile (logsDir ++ "/" ++ resultsFile) WriteMode $ \handle ->
    mapM_ (printStats handle) stats