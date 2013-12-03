import qualified Data.ByteString.Lazy.Char8 as BSL

main :: IO ()
main = do
	cnt <- BSL.readFile "big.txt"
	BSL.putStrLn cnt
