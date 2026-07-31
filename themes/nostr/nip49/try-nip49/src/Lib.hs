{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib (

	scrypt, decrypt, encryptUnsafeUnsafeForDebug,

	scryptIO,

	encryptDraft, exampleKey, exampleNonce, examplePlain

	) where

import Control.Monad
import Data.Maybe
import Data.Word
import Data.ByteString qualified as BS
import System.Entropy
import Crypto.Scrypt qualified as Scrypt
import Crypto.Error
import Crypto.Cipher.ChaChaPoly1305 qualified as CC

import Crypto.MAC.Poly1305 qualified as Mac

-- import Debug.Trace

encryptDraft :: BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
encryptDraft k n pln = fst
	$ CC.encrypt pln (either (error . show) id $ eitherCryptoError st)
	where
	st = CC.initializeX k
		(either (error . show) id . eitherCryptoError $ CC.nonce24 n)

decrypt ::
	BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString ->
	CryptoFailable BS.ByteString
decrypt ky nnc aad (splitAtR 16 -> (cph, etg)) = Mac.authTag etg >>= \etg' -> do
	st <- CC.finalizeAAD . CC.appendAAD aad
		<$> (CC.initializeX ky =<< CC.nonce24 nnc)
	let	(pln, CC.finalize -> ctg) = CC.decrypt cph st
	pln <$ when (ctg /= etg') (CryptoFailed CryptoError_MacKeyInvalid)

encryptUnsafeUnsafeForDebug k n aad pln = let
	(cp, st') = CC.encrypt pln (either (error . show) id $ eitherCryptoError st)
	Mac.Auth st'' = CC.finalize st'
	in
	(cp, st'')
	where
	st = CC.finalizeAAD . CC.appendAAD aad <$> CC.initializeX k
		(either (error . show) id . eitherCryptoError $ CC.nonce24 n)

splitAtR :: Int -> BS.ByteString -> (BS.ByteString, BS.ByteString)
splitAtR n bs = BS.splitAt (BS.length bs - n) bs

exampleKey, exampleNonce, examplePlain :: BS.ByteString
exampleKey = "1234567890abcdefghijklmnopqrstuv"
exampleNonce = "1234567890abcdefghijklmn"
examplePlain = "Hello, world!"

params :: Word8 -> Maybe Scrypt.ScryptParams
params logN = Scrypt.scryptParamsLen (fromIntegral logN) 8 1 32

scryptIO :: Word8 -> BS.ByteString -> IO Scrypted
scryptIO logN pss = do
	slt <- getEntropy 16
	pure Scrypted {
		salt = slt,
		pass = Scrypt.getHash $ Scrypt.scrypt
			(fromJust $ params logN) (Scrypt.Salt slt) (Scrypt.Pass pss) }

data Scrypted = Scrypted { salt :: BS.ByteString, pass :: BS.ByteString }
	deriving Show

scrypt :: Word8 -> BS.ByteString -> BS.ByteString -> BS.ByteString
scrypt lgn slt pss = Scrypt.getHash $ Scrypt.scrypt
	(fromJust $ params lgn) (Scrypt.Salt slt) (Scrypt.Pass pss)
