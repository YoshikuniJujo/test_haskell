{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module XChaCha (

	decrypt, encryptUnsafeUnsafeForDebug,

	encryptDraft

	) where

import Control.Monad
import Data.ByteString qualified as BS
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
