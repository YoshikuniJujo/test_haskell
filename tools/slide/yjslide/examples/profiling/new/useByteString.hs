import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
	cnt <- BS.readFile "big.txt"
	BS.putStrLn cnt
