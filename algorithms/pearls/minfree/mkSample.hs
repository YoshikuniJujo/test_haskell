
import Data.List
import Data.Word
import System.Random
import System.Environment

main :: IO ()
main = do
	s : _ <- getArgs
	writeFile ("sample" ++ s ++ ".txt")
		. unlines . map show . take (read s)
		. nub . (randomRs (0, maxBound) :: StdGen -> [Int]) $ mkStdGen 8
