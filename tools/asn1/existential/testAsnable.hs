{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import System.IO.Unsafe

import qualified Data.ByteString as BS

import Asnable

cert :: BS.ByteString
cert = unsafePerformIO $ BS.readFile "test_ASN_1_cert.der"

test :: BS.ByteString
test = "\x30\x80\x01\x01\x88\x00\x00"

data Rawest = RC Asn1Tag [Rawest] | RP Asn1Tag BS.ByteString
	deriving Show

toRawest :: AsnableBox -> Rawest
toRawest ab = case getAsn1Tag ab of
	t@(Asn1Tag _ Constructed _) -> let
		Just (RawConstructed _ as) = getAsnable ab in
		RC t $ map toRawest as
	t -> let
		Just (Raw _ bs) = getAsnable ab in
		RP t bs
