{-# LANGUAGE OverloadedStrings #-}

import System.IO.Unsafe

import qualified Data.ByteString as BS

import DecodeAsn1Common

cert :: BS.ByteString
cert = unsafePerformIO $ BS.readFile "test_ASN_1_cert.der"

under30Error, redundantTagNumberError :: BS.ByteString
under30Error = "\x1f\x1e"
redundantTagNumberError = "\x1f\x80\x7f"

main :: IO ()
main = print $ runAnalyzer decodeTag cert
