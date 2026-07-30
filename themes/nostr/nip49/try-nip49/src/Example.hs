{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Example (
	getChecked, symmetricKey, nsecDecrypt,

	encrypted, checksum, decrypted, hdr, vsn,

	encrypted', ct, st1234, dt, dt', hdrDt,

	checked, checked', encryptedRoundTrip, unbeck32
	) where

import Control.Arrow
import Control.Exception
import System.IO

import Data.Maybe
import Data.Word
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC

import Data.ByteArray qualified as BA

import Crypto.Error

import Lib
import BchEcc

import MyWords

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

-- decryptDraft :: String -> IO BS.ByteString
-- decryptDraft encryptedPrivateKey =

nsecDecrypt :: String -> IO String
nsecDecrypt enc = do
	pass <- withNoEcho getLine
	Just (_, [vn, [lgn], slt, nnc, ad, ct]) <- pure $ getChecked enc
--	print [vn, [lgn], slt, nnc, ad, ct]
	let	sk = getSymmetricKey lgn slt (BSC.pack pass)
	dt <- throwCryptoErrorIO $ decryptForDebug sk (BS.pack nnc) (BS.pack ad) (BS.pack ct)
	pure . bech32 "nsec" . word8sToWord5s $ BS.unpack dt

withNoEcho = bracket
	(hGetEcho stdin <* hSetEcho stdin False) (hSetEcho stdin) . const

getChecked :: String -> Maybe (String, [[Word8]])
getChecked = (((`toStructure` structure) `second`) <$>) . (checkedToHdrDat =<<) . (check =<<) . sepHrpDt

unbeck32 :: String -> Maybe (String, [Word8])
unbeck32 = (checkedToHdrDat =<<) . (check =<<) . sepHrpDt

encrypted :: String
encrypted = "ncryptsec1qgg9947rlpvqu76pj5ecreduf9jxhselq2nae2kghhvd5g7dgjtcxfqtd67p9m0w57lspw8gsq6yphnm8623nsl8xn9j4jdzz84zm3frztj3z7s35vpzmqf6ksu8r89qk5z2zxfmu5gv8th8wclt0h4p"

checksum = drop (length encrypted - 6) encrypted

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

structure = [1, 1, 16, 24, 1, 48]

toStructure xs [] = []
toStructure xs (n : ns) = d : toStructure r ns
	where (d, r) = splitAt n xs

getSymmetricKey :: Word8 -> [Word8] -> BS.ByteString -> BS.ByteString
getSymmetricKey lgn slt pass = scrypt (fromIntegral lgn) (BS.pack slt) pass

symmetricKey :: BS.ByteString
symmetricKey = case lgN of
	[lgn] -> getSymmetricKey lgn slt "nostr"
	_ -> error "never occur"

decrypted :: BS.ByteString
decrypted = throwCryptoError $ decryptForDebug symmetricKey (BS.pack nnc) (BS.pack ksb) (BS.pack ct)

encrypted' :: BS.ByteString
(encrypted', st1234) = encryptUnsafeUnsafeForDebug symmetricKey (BS.pack nnc) (BS.pack ksb) decrypted

dt' = vsn <> lgN <> slt <> nnc <> ksb <> BS.unpack encrypted' <> BA.unpack st1234

hdrDt = ("ncryptsec", dt')

checked' = Checked {
	checkedHumanReadPart = "ncryptsec",
	checkedDataPart = word8sToWord5s dt' }

encryptedRoundTrip = checkedToBech32 checked'

bech32 hrp dt = checkedToBech32 $ Checked hrp dt
