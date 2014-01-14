import System.Random
import System.Environment

import Pi

main :: IO ()
main = do
	n : _ <- getArgs
	g <- randomIO
	print $ getPi (read n) g
