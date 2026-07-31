{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Example (
	ncryptsecToNsec,

	encrypted, checksum, decrypted, vsn,

	encrypted', cipherText, st1234, dat, dt', hdrDt,

	checked, checked', encryptedRoundTrip, unbech32
	) where

import Control.Arrow
import Control.Exception
import System.IO

import Data.Word
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC

import Data.ByteArray qualified as BA

import Crypto.Error

import Lib qualified
import BchEcc

import MyWords

nsec, ncryptsec :: String
nsec = "nsec"
ncryptsec = "ncryptsec"

ncryptsecToNsec :: String -> IO String
ncryptsecToNsec =
	(bech32 nsec <$>) . either error decryptNcryptsec . unbech32 ncryptsec

decryptNcryptsec :: BS.ByteString -> IO BS.ByteString
decryptNcryptsec cs = withNoEcho getLine >>= \(BSC.pack -> pss) -> do
	[	[2], [lgn], BS.pack -> slt,
		BS.pack -> nnc, BS.pack -> aad, BS.pack -> ct ] <- dec cs
	let	sk = Lib.scrypt lgn slt pss
	throwCryptoErrorIO $ Lib.decrypt sk nnc aad ct
	where
	structure = [1, 1, 16, 24, 1, 48]
	dec = pure . (`go` structure) . BS.unpack
	go xs = \case
		[] -> []
		n : ns -> uncurry (:) . ((`go` ns) `second`) $ splitAt n xs

bech32 :: String -> BS.ByteString -> String
bech32 hrp dt = checkedToBech32 . Checked hrp . word8sToWord5s $ BS.unpack dt

unbech32 :: String -> String -> Either String BS.ByteString
unbech32 hrp0 = (BS.pack <$>) . (checkedToHdrDat =<<) . (check hrp0 =<<) . sepHrpDt

encrypted :: String
encrypted = "ncryptsec1qgg9947rlpvqu76pj5ecreduf9jxhselq2nae2kghhvd5g7dgjtcxfqtd67p9m0w57lspw8gsq6yphnm8623nsl8xn9j4jdzz84zm3frztj3z7s35vpzmqf6ksu8r89qk5z2zxfmu5gv8th8wclt0h4p"

checksum :: String
checksum = drop (length encrypted - 6) encrypted

checked :: Either String Checked
checked = check ncryptsec =<< sepHrpDt encrypted

dat :: [Word8]
dat = either error id $ checkedToHdrDat =<< checked

vsn, lgN, salt, nonce, ksb, r0, r1, r2, r3, cipherText :: [Word8]
(vsn, r0) = splitAt 1 dat
(lgN, r1) = splitAt 1 r0
(salt, r2) = splitAt 16 r1
(nonce, r3) = splitAt 24 r2
(ksb, cipherText) = splitAt 1 r3

symmKey :: BS.ByteString
symmKey = case lgN of
	[lgn] -> Lib.scrypt lgn (BS.pack salt) "nostr"
	_ -> error "never occur"

decrypted :: BS.ByteString
decrypted = throwCryptoError $ Lib.decrypt symmKey (BS.pack nonce) (BS.pack ksb) (BS.pack cipherText)

encrypted' :: BS.ByteString
st1234 :: BA.Bytes
(encrypted', st1234) = Lib.encryptUnsafeUnsafeForDebug symmKey (BS.pack nonce) (BS.pack ksb) decrypted

dt' :: [Word8]
dt' = vsn <> lgN <> salt <> nonce <> ksb <> BS.unpack encrypted' <> BA.unpack st1234

hdrDt :: (String, [Word8])
hdrDt = ("ncryptsec", dt')

checked' :: Checked
checked' = Checked {
	checkedHumanReadPart = "ncryptsec",
	checkedDataPart = word8sToWord5s dt' }

encryptedRoundTrip :: String
encryptedRoundTrip = checkedToBech32 checked'

withNoEcho :: IO a -> IO a
withNoEcho = bracket
	(hGetEcho stdin <* hSetEcho stdin False) (hSetEcho stdin) . const
