{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Ncryptsec where

import Control.Arrow
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Text qualified as T
import Crypto.Error

import Lib qualified
import Scrypt qualified
import Bech32 qualified

nsec, ncryptsec :: String
nsec = "nsec"; ncryptsec = "ncryptsec"

toNsec :: MonadFail m => m String -> T.Text -> m T.Text
toNsec gp = (bech32 nsec <$>) . either fail (decrypt gp) . unbech32 ncryptsec

decrypt :: MonadFail m => m String -> BS.ByteString -> m BS.ByteString
decrypt gp cs = gp >>= \(BSC.pack -> pss) -> do
	[	[2], [lgn], BS.pack -> slt,
		BS.pack -> nnc, BS.pack -> aad, BS.pack -> ct ] <- dec cs
	either (fail . show) pure . eitherCryptoError
		$ Lib.decrypt (Scrypt.hash lgn slt pss) nnc aad ct
	where
	dec = pure . (`go` structure) . BS.unpack
	structure = [1, 1, 16, 24, 1, 48]
	go xs = \case
		[] -> []
		n : ns -> uncurry (:) . ((`go` ns) `second`) $ splitAt n xs

bech32 :: String -> BS.ByteString -> T.Text
bech32 hrp = Bech32.encode . Bech32.fromByteString hrp

unbech32 :: String -> T.Text -> Either String BS.ByteString
unbech32 hrp0 = (Bech32.toByteString hrp0 =<<) .  Bech32.decode
