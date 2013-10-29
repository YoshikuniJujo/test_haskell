import System.Random

main :: IO ()
main = do
	g <- getStdGen
	print $ take 10 $ randomRs (1 :: Int, 6) g
