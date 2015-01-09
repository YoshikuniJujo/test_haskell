import System.Random
import Control.Applicative

main :: IO ()
main = do
	x <- randomRIO (1, 10)
	putStrLn "数字を当ててください(1 から 10 までの整数)"
	loop x

loop :: Int -> IO ()
loop x = do
	n <- readLn
	case compare x n of
		LT -> do
			putStrLn "もっと小さい"
			loop x
		GT -> do
			putStrLn "もっと大きい"
			loop x
		_ -> putStrLn "当たり"
