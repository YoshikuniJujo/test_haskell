{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Example (
	Ncryptsec.toNsec,

	encrypted, checksum, decrypted, vsn,

	encrypted', cipherText, st1234, dat, dt', hdrDt,

	checked, checked', encryptedRoundTrip,
	) where

import Data.Word
import Data.ByteString qualified as BS
import Data.Text qualified as T

import Data.ByteArray qualified as BA

import Crypto.Error

import XChaCha qualified
import Bech32 qualified

import MyWords

import Ncryptsec qualified
import Scrypt qualified

encrypted :: T.Text
encrypted =
	"ncryptsec1" <>
	"qgg9947rlpvqu76pj5ecreduf9jxhselq2nae2kghhvd5g7dgjtcxfqtd67p9m0w" <>
	"57lspw8gsq6yphnm8623nsl8xn9j4jdzz84zm3frztj3z7s35vpzmqf6ksu8r89q" <>
	"k5z2zxfmu5gv8th8wclt0h4p"

checksum :: T.Text
checksum = T.drop (T.length encrypted - 6) encrypted

checked :: Either String Bech32.B
checked = Bech32.check =<< Bech32.sepHrpDt (T.unpack encrypted)

dat :: [Word8]
dat = either error id $ Bech32.checkedToHdrDat Ncryptsec.ncryptsec =<< checked

vsn, lgN, salt, nonce, ksb, r0, r1, r2, r3, cipherText :: [Word8]
(vsn, r0) = splitAt 1 dat
(lgN, r1) = splitAt 1 r0
(salt, r2) = splitAt 16 r1
(nonce, r3) = splitAt 24 r2
(ksb, cipherText) = splitAt 1 r3

symmKey :: BS.ByteString
symmKey = case lgN of
	[lgn] -> Scrypt.hash lgn (BS.pack salt) "nostr"
	_ -> error "never occur"

decrypted :: BS.ByteString
decrypted = throwCryptoError $ XChaCha.decrypt symmKey (BS.pack nonce) (BS.pack ksb) (BS.pack cipherText)

encrypted' :: BS.ByteString
st1234 :: BA.Bytes
(encrypted', st1234) = XChaCha.encryptUnsafeUnsafeForDebug symmKey (BS.pack nonce) (BS.pack ksb) decrypted

dt' :: [Word8]
dt' = vsn <> lgN <> salt <> nonce <> ksb <> BS.unpack encrypted' <> BA.unpack st1234

hdrDt :: (String, [Word8])
hdrDt = ("ncryptsec", dt')

checked' :: Bech32.B
checked' = Bech32.B {
	Bech32.checkedHumanReadPart = "ncryptsec",
	Bech32.checkedDataPart = word8sToWord5s dt' }

encryptedRoundTrip :: T.Text
encryptedRoundTrip = Bech32.encode checked'
