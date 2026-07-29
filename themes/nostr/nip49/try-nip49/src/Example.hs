{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Example (
	decrypted, hdr, vsn,

	encrypted', ct, st1234, dt, dt'
	) where

import Data.Maybe
import Data.Word
import Data.ByteString qualified as BS

import Data.ByteArray qualified as BA

import Crypto.Error

import Lib
import BchEcc

{-
password = "nostr"

logN = 16

getSaltSymmetricKey = scryptIO logN password

example = do
	putStrLn $ "PASSWORD = " ++ show password
	putStrLn $ "LOG_N = " ++ show logN
	Scrypted { salt = slt, pass = sk } <- getSaltSymmetricKey
	putStrLn $ "SALT = " ++ show slt
	putStrLn $ "SYMMETRIC_KEY = " ++ show sk
	-}

encrypted :: String
encrypted = "ncryptsec1qgg9947rlpvqu76pj5ecreduf9jxhselq2nae2kghhvd5g7dgjtcxfqtd67p9m0w57lspw8gsq6yphnm8623nsl8xn9j4jdzz84zm3frztj3z7s35vpzmqf6ksu8r89qk5z2zxfmu5gv8th8wclt0h4p"

checked :: Maybe Checked
checked = check =<< sepHrpDt encrypted

hdr :: String
dt :: [Word8]
(hdr, dt) = fromJust $ checkedToHdrDat =<< checked

vsn, lgN, slt, nnc, ksb, r0, r1, r2, r3, ct :: [Word8]
(vsn, r0) = splitAt 1 dt
(lgN, r1) = splitAt 1 r0
(slt, r2) = splitAt 16 r1
(nnc, r3) = splitAt 24 r2
(ksb, ct) = splitAt 1 r3

symmetricKey :: BS.ByteString
symmetricKey = case lgN of
	[lgn] -> scrypt (fromIntegral lgn) (BS.pack slt) "nostr"
	_ -> error "never occur"

decrypted :: BS.ByteString
decrypted = throwCryptoError $ decryptForDebug symmetricKey (BS.pack nnc) (BS.pack ksb) (BS.pack ct)

encrypted' :: BS.ByteString
(encrypted', st1234) = encryptUnsafeUnsafeForDebug symmetricKey (BS.pack nnc) (BS.pack ksb) decrypted

dt' = vsn <> lgN <> slt <> nnc <> ksb <> BS.unpack encrypted' <> BA.unpack st1234
