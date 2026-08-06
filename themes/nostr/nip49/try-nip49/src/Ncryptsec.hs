{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Ncryptsec where

import Control.Arrow
import Data.Word
import Data.ByteArray qualified as BA
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Text qualified as T
import System.Entropy
import Crypto.Error

import XChaCha qualified
import Scrypt qualified
import Bech32 qualified

nsec, ncryptsec :: String
nsec = "nsec"; ncryptsec = "ncryptsec"

toNsec :: MonadFail m => m String -> T.Text -> m T.Text
toNsec gp = (Bech32.encode . Bech32.fromByteString nsec <$>)
	. either fail (decrypt gp)
	. (Bech32.toByteString ncryptsec =<<) . Bech32.decode

decrypt :: MonadFail m => m String -> BS.ByteString -> m BS.ByteString
decrypt gp cs = gp >>= \(BSC.pack -> pss) -> do
	[	[2], [lgn], BS.pack -> slt,
		BS.pack -> nnc, BS.pack -> aad, BS.pack -> ct ] <- dec cs
	either (fail . show) pure . eitherCryptoError
		$ XChaCha.decrypt (Scrypt.hash lgn slt pss) nnc aad ct
	where
	dec = pure . (`go` structure) . BS.unpack
	structure = [1, 1, 16, 24, 1, 48]
	go xs = \case
		[] -> []
		n : ns -> uncurry (:) . ((`go` ns) `second`) $ splitAt n xs

fromNSec :: Word8 -> Word8 -> IO String -> T.Text -> IO T.Text
fromNSec lgn ksb gp = (Bech32.encode . Bech32.fromByteString ncryptsec <$>)
	. either fail (encrypt lgn ksb gp)
	. (Bech32.toByteString nsec =<<) . Bech32.decode

encrypt :: Word8 -> Word8 -> IO String -> BS.ByteString -> IO BS.ByteString
encrypt lgn ksb gp pln = gp >>= \(BSC.pack -> pss) -> do
	(slt, ky) <- symmKey lgn pss
	let	aad = BS.pack [ksb]
	(nnc, ct, mac) <- XChaCha.encrypt ky aad pln
	pure $ build 2 lgn slt nnc aad ct (BA.convert mac)

symmKey :: Word8 -> BS.ByteString -> IO (BS.ByteString, BS.ByteString)
symmKey lgn pss = do
	slt <- getEntropy 16
	pure (slt, Scrypt.hash lgn slt pss)

build :: Word8 -> Word8 ->
	BS.ByteString -> BS.ByteString -> BS.ByteString ->
	BS.ByteString -> BS.ByteString -> BS.ByteString
build vn lgn slt nnc aad ct mac = vn `BS.cons` lgn `BS.cons` slt <> nnc <> aad <> ct <> mac
