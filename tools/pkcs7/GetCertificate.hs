{-# LANGUAGE OverloadedStrings #-}

module GetCertificate (
	fromPem, toPem,
	fromZigbert
	) where

import Data.PEM
import Data.ASN1.Types
import Data.ASN1.Error
import Data.ASN1.Encoding
import Data.ASN1.BinaryEncoding

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Asn1Container

fromPem :: BS.ByteString -> Either String [ASN1]
fromPem = either
	(Left . show)
	(either (Left . show) (Right . concat) . mapM pemToAsn1) . pemParseBS

pemToAsn1 :: PEM -> Either ASN1Error [ASN1]
pemToAsn1 = decodeASN1 DER . LBS.fromChunks . (: []) . pemContent

toPem :: [ASN1] -> BS.ByteString
toPem = ("Number of certificates: 3\n\n" `BS.append`)
	. BS.intercalate "\n" . map (toPem1 . writeAsn1Container . (: []))
	. fst . parseAsn1Container

toPem1 :: [ASN1] -> BS.ByteString
toPem1 a = pemWriteBS PEM {
	pemName = "CERTIFICATE",
	pemHeader = [],
	pemContent = LBS.toStrict $ encodeASN1 DER a }

fromZigbert :: BS.ByteString -> Either String [ASN1]
fromZigbert z = case decodeASN1 BER (LBS.fromStrict z) of
	Right a -> let
		([CntSequence [_, CntContext 0 [bd0]]], []) = parseAsn1Container a
		CntSequence [_, _, _, CntContext 0 crts, _] = bd0 in
		Right $ writeAsn1Container crts
	Left e -> Left $ show e
