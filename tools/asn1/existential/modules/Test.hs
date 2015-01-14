{-# LANGUAGE OverloadedStrings #-}

module Test (
	module Ber,
	toRw, cert, indTest, seqBoolTest, heteroTest1, heteroTest2,
	) where

import System.IO.Unsafe

import qualified Data.ByteString as BS

import Ber

cert :: BS.ByteString
cert = unsafePerformIO $ BS.readFile "test_ASN_1_cert.der"

indTest :: BS.ByteString
indTest = "\x30\x80\x01\x01\x88\x00\x00"

seqBoolTest :: BS.ByteString
seqBoolTest = "0\t\SOH\SOH\NUL\SOH\SOH\255\SOH\SOH\255"

heteroTest1, heteroTest2 :: BS.ByteString
heteroTest1 =
	"0\SO\STX\STX\SOH\NUL\SOH\SOH\NUL\STX\STX\NUL\255\SOH\SOH\255"
heteroTest2 =
	"0\128\STX\STX\SOH\NUL\SOH\SOH\NUL\STX\STX\NUL\255\SOH\SOH\255\NUL\NUL"

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
