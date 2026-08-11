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
	. (uncurry encode <$>) . either error (encrypt lgn ksb gp)
	. (Bech32.getData nsec =<<) . Bech32.decode

encrypt :: Word8 -> Word8 -> IO String -> BS.ByteString ->
	IO (SymKeyParams, Encrypted)
encrypt lgn ksb@((BS.pack . (: [])) -> aad) gp pln = gp >>= \(BSC.pack -> pss) -> do
	(slt, ky) <- skey pss
	(nnc, ct, BA.convert -> mac) <- XChaCha.encrypt ky aad pln
	pure (	SymKeyParams {
			symKeyParamsLogN = lgn,
			symKeyParamsSalt = slt },
		Encrypted {
			encryptedVersion = 2,
			encryptedNonce = nnc,
			encryptedKeySecurityByte = ksb,
			encryptedCipherText = ct,
			encryptedMac = mac } )
	where skey pss = (id &&& \s -> Scrypt.hash lgn s pss) <$> getEntropy 16

toNsec :: MonadFail m => m String -> T.Text -> m T.Text
toNsec gp = (Bech32.encode . Bech32.B nsec <$>) . either fail decrypt'
	. (Bech32.getData ncryptsec =<<) . Bech32.decode
	where
	decrypt' cs = do
		(skp, ec) <- maybe (fail "bad") pure $ decode cs
		decrypt gp skp ec

decrypt :: MonadFail m => m String -> SymKeyParams -> Encrypted -> m BS.ByteString
decrypt gp skp ec = gp >>= \(BSC.pack -> pss) -> do
	2 <- pure $ encryptedVersion ec
	let	lgn = symKeyParamsLogN skp
		slt = symKeyParamsSalt skp
		nnc = encryptedNonce ec
		aad = BS.pack [encryptedKeySecurityByte ec]
		ct = encryptedCipherText ec
		mac = encryptedMac ec
	either (fail . show) pure . eitherCryptoError
		$ XChaCha.decrypt' (Scrypt.hash lgn slt pss) nnc aad ct mac

data SymKeyParams = SymKeyParams {
	symKeyParamsLogN :: Word8, symKeyParamsSalt :: BS.ByteString }
	deriving Show

data Encrypted = Encrypted {
	encryptedVersion :: Word8, encryptedNonce :: BS.ByteString,
	encryptedKeySecurityByte :: Word8, encryptedCipherText :: BS.ByteString,
	encryptedMac :: BS.ByteString }
	deriving Show

encode :: SymKeyParams -> Encrypted -> BS.ByteString
encode skp ec =
	encryptedVersion ec `BS.cons` symKeyParamsLogN skp `BS.cons`
	symKeyParamsSalt skp <> encryptedNonce ec <>
	(encryptedKeySecurityByte ec `BS.cons` encryptedCipherText ec) <>
	encryptedMac ec

decode :: BS.ByteString -> Maybe (SymKeyParams, Encrypted)
decode bs = do
	[	[vsn], [lgn], BS.pack -> slt, BS.pack -> nnc,
		[ksb], BS.pack -> ct, BS.pack -> mac ] <- pure $ dec bs
	pure (	SymKeyParams { symKeyParamsLogN = lgn, symKeyParamsSalt = slt },
		Encrypted {
			encryptedVersion = vsn, encryptedNonce = nnc,
			encryptedKeySecurityByte = ksb, encryptedCipherText = ct,
			encryptedMac = mac } )
	where
	dec = (`go` structure') . BS.unpack
	structure' = [1, 1, 16, 24, 1, 32, 16]
	go xs = \case
		[] -> []
		n : ns -> uncurry (:) . ((`go` ns) `second`) $ splitAt n xs
