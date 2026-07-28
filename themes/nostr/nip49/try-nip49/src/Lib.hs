{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.ByteString qualified as BS
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
