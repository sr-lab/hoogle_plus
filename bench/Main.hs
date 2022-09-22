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

exercises :: [(String, String, [Example])]
exercises = [
  ("firstRight", "[Either a b] -> Either a b", []),
  ("firstKey", "[(a, b)] -> a", []),
  ("flatten", "[[[a]]] -> [a]", []),
  ("repl-funcs", "(a -> b) -> Int -> [a -> b]", []),
  ("containsEdge", "Int -> (Int, Int) -> Bool", []),
  ("multiApp", "(a -> b -> c) -> a -> c", []),
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
  ("singleList", "Int -> [Int]", [])
  ]

-- example with 
--("tail", "[Int] -> [Int]", [Example {inputs = ["[1, 2, 3]"], output = "[2, 3]"}])

execExercise :: (String, String, [Example]) -> IO (String, Maybe Double) -- total time of first solution
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