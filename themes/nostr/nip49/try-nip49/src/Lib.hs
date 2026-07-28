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

someFunc :: IO ()
someFunc = putStrLn "someFunc"

encrypt k n pln = fst
	$ ChaCha.encrypt pln (either (error . show) id $ eitherCryptoError st)
	where
	st = ChaCha.initializeX k
		(either (error . show) id . eitherCryptoError $ ChaCha.nonce24 n)

decrypt k n pln = fst
	$ ChaCha.decrypt pln (either (error . show) id $ eitherCryptoError st)
	where
	st = ChaCha.initializeX k
		(either (error . show) id . eitherCryptoError $ ChaCha.nonce24 n)

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
