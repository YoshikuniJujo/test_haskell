import System.IO.Unsafe

import qualified Data.ByteString as BS

import DecodeAsn1Common

cert :: BS.ByteString
cert = unsafePerformIO $ BS.readFile "test_ASN_1_cert.der"

main :: IO ()
main = print $ runAnalyzer decodeTag cert
