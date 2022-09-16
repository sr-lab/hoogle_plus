import System.Process (readCreateProcessWithExitCode, shell)
import System.IO (openFile, hGetLine, hClose, IOMode(ReadMode))
import Text.Printf (printf)

exercises :: [(String, String)]
exercises = [("[Int] -> [Int]", "[([[1, 2, 3]], [2, 3, 4])]")]


execExercise :: String -> String -> String -> IO ( Double -- ghc
                                                 , Double -- match
                                                 , Double -- solver
                                                 )
execExercise ty exs log = do
  -- run exercise
  let cmd = printf "timeout 60s stack exec -- hplus \"%s\" --example=\"%s\" --cnt=100 --out=%s" ty exs log 
  readCreateProcessWithExitCode (shell cmd) ""
  -- read results
  f <- openFile log ReadMode
  hGetLine f -- read solution
  statsLine <- hGetLine f -- read stats
  hClose f
  let stats = (map read $ words statsLine) :: [Double]
  return (stats !! 0, stats !! 1, stats !! 2)

main :: IO ()
main = do
  let withIndex = zipWith (\(x, y) z -> (x, y, z)) exercises ((iterate (+1) 1) :: [Int])
  stats <- mapM (\(x, y, z) -> execExercise x y ("bench-logs/" ++ show z ++ ".log")) withIndex
  print stats