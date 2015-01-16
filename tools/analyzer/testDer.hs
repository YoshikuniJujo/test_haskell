import System.IO.Unsafe (unsafePerformIO)

import qualified Data.ByteString as BS

import Der
import Analyzer

cert :: BS.ByteString
cert = unsafePerformIO $ BS.readFile "test_ASN_1_cert.der"
