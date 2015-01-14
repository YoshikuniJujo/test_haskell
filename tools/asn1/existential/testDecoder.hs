{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import System.IO.Unsafe

import qualified Data.ByteString as BS

import Decoder

cert :: BS.ByteString
cert = unsafePerformIO $ BS.readFile "test_ASN_1_cert.der"

ind_test :: BS.ByteString
ind_test = "\x30\x80\x01\x01\x88\x00\x00"

data Rw = RC Asn1Tag [Rw] | RP Asn1Tag BS.ByteString
	deriving Show

toRw :: BerDecodeBox -> Rw
toRw ab = case getAsn1Tag ab of
	t@(Asn1Tag _ Constructed _) -> let
		Just (RawConstructed _ as) = getBerDecode ab in
		RC t $ map toRw as
	t -> let
		Just (Raw _ bs) = getBerDecode ab in
		RP t bs

main :: IO ()
main = let
	Right (ab, "") = runAnalyzer (decodeWith [
		decodeRule (undefined :: RawConstructed),
		decodeRule (undefined :: Raw)]) ind_test in
	print $ toRw ab
