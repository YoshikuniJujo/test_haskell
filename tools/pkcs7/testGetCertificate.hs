import Control.Applicative
import System.Environment

import qualified Data.ByteString as BS

import GetCertificate
import Asn1Container

main :: IO ()
main = do
	fppm : fpzb : _ <- getArgs
	pm <- BS.readFile fppm
	zb <- fromZigbert <$> BS.readFile fpzb
	print $ zb == fromPem pm
	print $ Right pm == either Left (Right . toPem) (fromPem pm)
