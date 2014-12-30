{-# LANGUAGE OverloadedStrings #-}

import System.IO.Unsafe

import qualified Data.ByteString as BS

import DecodeAsn1Common

cert :: BS.ByteString
cert = unsafePerformIO $ BS.readFile "test_ASN_1_cert.der"

tagExam1 :: BS.ByteString
tagExam1 = "\x1f\x85\x05"

cert1 :: BS.ByteString
Just (Asn1 _ cert1, "") = decode1 cert
