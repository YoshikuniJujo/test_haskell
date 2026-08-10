{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module XChaCha (decrypt, encrypt) where

import Control.Monad
import Data.ByteArray qualified as BA
import Data.ByteString qualified as BS
import System.Entropy
import Crypto.Error
import Crypto.Cipher.ChaChaPoly1305 qualified as CC
import Crypto.MAC.Poly1305 qualified as Mac

import Tools

encrypt :: BS.ByteString -> BS.ByteString -> BS.ByteString ->
	IO (BS.ByteString, BS.ByteString, BA.Bytes)
encrypt ky aad pln = do
	nnc_ <- getEntropy 24
	st <- finAppendAAD aad
		<$> throwCryptoErrorIO (CC.initializeX ky =<< CC.nonce24 nnc_)
	let	(cs, CC.finalize -> Mac.Auth mac) = CC.encrypt pln st
	pure (nnc_, cs, mac)

decrypt :: BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString ->
	CryptoFailable BS.ByteString
decrypt ky nnc aad (splitAtR 16 -> (cph, etg)) = Mac.authTag etg >>= \etg' -> do
	st <- finAppendAAD aad <$> (CC.initializeX ky =<< CC.nonce24 nnc)
	let	(pln, CC.finalize -> ctg) = CC.decrypt cph st
	pln <$ when (ctg /= etg') (CryptoFailed CryptoError_MacKeyInvalid)

finAppendAAD :: BA.ByteArrayAccess ba => ba -> CC.State -> CC.State
finAppendAAD aad = CC.finalizeAAD . CC.appendAAD aad
