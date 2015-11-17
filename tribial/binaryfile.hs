import System.Environment

import qualified Data.ByteString as BS

main :: IO ()
main = do
	fp : _ <- getArgs
	c <- BS.readFile fp
	BS.putStr c
