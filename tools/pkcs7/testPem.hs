import System.Environment
import Data.PEM
import Data.ASN1.Types
import Data.ASN1.Error
import Data.ASN1.Encoding
import Data.ASN1.BinaryEncoding

import qualified Data.ByteString.Lazy as LBS

main :: IO ()
main = do
	fp : _ <- getArgs
	cnt <- LBS.readFile fp
	let	cs = pemParseLBS cnt
	print . concat . (\(Right r) -> r) $ either
		(Left . show)
		(mapLeft show . mapEither . map pemToAsn1) cs

pemToAsn1 :: PEM -> Either ASN1Error [ASN1]
pemToAsn1 = decodeASN1 DER . LBS.fromChunks . (: []) . pemContent

mapLeft :: (l -> l') -> Either l r -> Either l' r
mapLeft f (Left l) = Left $ f l
mapLeft _ (Right r) = Right r

mapEither :: [Either a b] -> Either a [b]
mapEither [] = return []
mapEither (e : es) = do
	x <- e
	xs <- mapEither es
	return $ x : xs
