{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib (

	scrypt, decryptForDebug,

	scryptIO,

	encryptDraft, exampleKey, exampleNonce, examplePlain

	) where

import Data.Maybe
import Data.ByteString qualified as BS
import System.Entropy
import Crypto.Scrypt qualified as Scrypt
import Crypto.Error
import Crypto.Cipher.ChaChaPoly1305 qualified as CC

import Crypto.MAC.Poly1305 qualified as Mac

import Debug.Trace

encryptDraft :: BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
encryptDraft k n pln = fst
	$ CC.encrypt pln (either (error . show) id $ eitherCryptoError st)
	where
	st = CC.initializeX k
		(either (error . show) id . eitherCryptoError $ CC.nonce24 n)

decryptForDebug ::
	BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString ->
	CryptoFailable BS.ByteString
decryptForDebug k n aad (splitAtRight 16 -> (cph, etg)) = do
	st <- CC.finalizeAAD . CC.appendAAD aad
		<$> (CC.initializeX k =<< CC.nonce24 n)
	let	(pln, CC.finalize -> Mac.Auth ctg) = CC.decrypt cph st
	pln <$ traces [show etg, show ctg]

splitAtRight :: Int -> BS.ByteString -> (BS.ByteString, BS.ByteString)
splitAtRight n bs = BS.splitAt (BS.length bs - n) bs

traces :: Monad m => [String] -> m ()
traces = foldr (\s a -> trace s (pure ()) >> a) (pure ())

exampleKey, exampleNonce, examplePlain :: BS.ByteString
exampleKey = "1234567890abcdefghijklmnopqrstuv"
exampleNonce = "1234567890abcdefghijklmn"
examplePlain = "Hello, world!"

params :: Integer -> Maybe Scrypt.ScryptParams
params logN = Scrypt.scryptParamsLen logN 8 1 32

scryptIO :: Integer -> BS.ByteString -> IO Scrypted
scryptIO logN pss = do
	slt <- getEntropy 16
	pure Scrypted {
		salt = slt,
		pass = Scrypt.getHash $ Scrypt.scrypt
			(fromJust $ params logN) (Scrypt.Salt slt) (Scrypt.Pass pss) }

data Scrypted = Scrypted { salt :: BS.ByteString, pass :: BS.ByteString }
	deriving Show

scrypt :: Integer -> BS.ByteString -> BS.ByteString -> BS.ByteString
scrypt lgn slt pss = Scrypt.getHash $ Scrypt.scrypt
	(fromJust $ params lgn) (Scrypt.Salt slt) (Scrypt.Pass pss)
