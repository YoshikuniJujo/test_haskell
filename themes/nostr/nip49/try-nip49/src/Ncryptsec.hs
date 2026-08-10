{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Codec.Bech32 qualified as Bech32

nsec, ncryptsec :: String
nsec = "nsec"; ncryptsec = "ncryptsec"

fromNsec :: Word8 -> Word8 -> IO String -> T.Text -> IO T.Text
fromNsec lgn ksb gp = (Bech32.encode . Bech32.B ncryptsec <$>)
	. either error (encrypt lgn ksb gp)
	. (Bech32.getData nsec =<<) . Bech32.decode

encrypt :: Word8 -> Word8 -> IO String -> BS.ByteString -> IO BS.ByteString
encrypt lgn ((BS.pack . (: [])) -> aad) gp pln = gp >>= \(BSC.pack -> pss) -> do
	(slt, ky) <- skey pss
	(nnc, ct, BA.convert -> mac) <- XChaCha.encrypt ky aad pln
	pure $ 2 `BS.cons` lgn `BS.cons` slt <> nnc <> aad <> ct <> mac
	where skey pss = (id &&& \s -> Scrypt.hash lgn s pss) <$> getEntropy 16

toNsec :: MonadFail m => m String -> T.Text -> m T.Text
toNsec gp = (Bech32.encode . Bech32.B nsec <$>) . either fail (decrypt gp)
	. (Bech32.getData ncryptsec =<<) . Bech32.decode

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
