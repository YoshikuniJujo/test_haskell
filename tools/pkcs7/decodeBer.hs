import Data.ASN1.Encoding
import Data.ASN1.BinaryEncoding
import System.Environment

import qualified Data.ByteString.Lazy as LBS

main :: IO ()
main = do
	fp : _ <- getArgs
	cnt <- LBS.readFile fp
	print $ decodeASN1 BER cnt
