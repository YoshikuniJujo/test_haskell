{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.Maybe
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Base64 qualified as B64
import System.Entropy
import Crypto.Scrypt qualified as Scrypt
import Crypto.Error
import Crypto.Cipher.ChaChaPoly1305 qualified as ChaCha

import Crypto.MAC.Poly1305 qualified as Mac

import Debug.Trace

someFunc :: IO ()
someFunc = putStrLn "someFunc"

encrypt k n pln = fst
	$ ChaCha.encrypt pln (either (error . show) id $ eitherCryptoError st)
	where
	st = ChaCha.initializeX k
		(either (error . show) id . eitherCryptoError $ ChaCha.nonce24 n)

decrypt k n aad cph = let
	(cph', exp) = splitCph cph
	(pln, st') = ChaCha.decrypt cph' (either (error . show) id $ eitherCryptoError st)
	ctg = ChaCha.finalize st'
	Mac.Auth ctg' = ctg
	in
	trace (show exp ++ show ctg') . trace (show (BS.length cph')) . trace (show (BS.length exp))
		$ pln
	where
	st = ChaCha.finalizeAAD . ChaCha.appendAAD aad <$> ChaCha.initializeX k
		(either (error . show) id . eitherCryptoError $ ChaCha.nonce24 n)

splitCph ::BS.ByteString -> (BS.ByteString, BS.ByteString)
splitCph cph = BS.splitAt (BS.length cph - 16) cph

exampleKey, exampleNonce, examplePlain :: BS.ByteString
exampleKey = "1234567890abcdefghijklmnopqrstuv"
exampleNonce = "1234567890abcdefghijklmn"
examplePlain = "Hello, world!"

params logN = Scrypt.scryptParamsLen logN 8 1 32

scryptIO logN pss = do
	slt <- getEntropy 16
	pure Scrypted {
		salt = slt,
		pass = Scrypt.getHash $ Scrypt.scrypt
			(fromJust $ params logN) (Scrypt.Salt slt) (Scrypt.Pass pss) }

data Scrypted = Scrypted { salt :: BS.ByteString, pass :: BS.ByteString }
	deriving Show

scrypt lgn slt pss = Scrypt.getHash $ Scrypt.scrypt
	(fromJust $ params lgn) (Scrypt.Salt slt) (Scrypt.Pass pss)
