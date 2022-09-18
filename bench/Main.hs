import System.Process (readCreateProcessWithExitCode, shell)
import System.IO (openFile, hGetLine, hClose, IOMode(ReadMode))
import Text.Printf (printf)
import System.Directory (removePathForcibly, createDirectory)

exercises :: [(String, Maybe String)]
exercises = [("[Int] -> [Int]", Just "[([[1, 2, 3]], [2, 3, 4])]")]

execExercise :: String -> Maybe String -> String -> IO (Double, Double) -- match, total
execExercise ty mexs log = do
  -- run exercise

  readCreateProcessWithExitCode (shell $ command ty mexs) ""
  putStrLn $ command ty mexs
  -- read results
  f <- openFile log ReadMode
  hGetLine f -- read solution
  statsLine <- hGetLine f -- read stats
  hClose f
  let stats = (map read $ words statsLine) :: [Double]
  return (stats !! 0, stats !! 1)
  where
    command :: String -> Maybe String -> String
    command ty Nothing = printf "timeout 60s stack exec -- hplus \"%s\" --cnt=100 --out=%s" ty log
    command ty (Just exs) = printf "timeout 60s stack exec -- hplus \"%s\" --example=\"%s\" --cnt=100 --out=%s" ty exs log 

main :: IO ()
main = do
  removePathForcibly "bench-logs"
  createDirectory "bench-logs"

  let withIndex = zipWith (\(x, y) z -> (x, y, z)) exercises ((iterate (+1) 1) :: [Int])
  stats <- mapM (\(x, y, z) -> execExercise x y ("bench-logs/" ++ show z ++ ".log")) withIndex
  print stats