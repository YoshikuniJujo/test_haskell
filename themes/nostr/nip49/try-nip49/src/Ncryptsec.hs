{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

nsec, ncsec :: String
nsec = "nsec"; ncsec = "ncryptsec"

fromNsec :: Word8 -> Word8 -> IO String -> T.Text -> IO T.Text
fromNsec lgn ks gp = (Bech32.encode . Bech32.B ncsec <$>) . (uncurry encode <$>)
	. (encrypt lgn ks gp =<<) . (Bech32.getData nsec =<<) . Bech32.decode

encrypt :: Word8 -> Word8 -> IO String -> BS.ByteString -> IO Ncryptsec
encrypt lgn ks@(BS.singleton -> aad) gp pln = gp >>= \(BSC.pack -> pss) -> do
	(slt, ky) <- skey pss
	(nnc, ct, BA.convert -> mac) <- XChaCha.encrypt ky aad pln
	pure (	SymKeyPrms { symKeyPrmsLogN = lgn, symKeyPrmsSalt = slt },
		Encrypted {
			encryptedVersion = 2, encryptedNonce = nnc,
			encryptedKeySecurityByte = ks,
			encryptedCipherText = ct, encryptedMac = mac } )
	where skey pss = (id &&& \s -> Scrypt.hash lgn s pss) <$> getEntropy 16

toNsec :: MonadFail m => m String -> T.Text -> m T.Text
toNsec gp = (Bech32.encode . Bech32.B nsec <$>) . (uncurry (decrypt gp) =<<)
	. (decode =<<) . (Bech32.getData ncsec =<<) . Bech32.decode

decrypt :: MonadFail m => m String -> SymKeyPrms -> Encrypted -> m BS.ByteString
decrypt gp skp ec = gp >>= \(BSC.pack -> pss) -> do
	2 <- pure vsn
	either (fail . show) pure . eitherCryptoError
		$ XChaCha.decrypt (Scrypt.hash lgn slt pss) nnc aad ct mac
	where
	SymKeyPrms { symKeyPrmsLogN = lgn, symKeyPrmsSalt = slt } = skp
	Encrypted {
		encryptedVersion = vsn, encryptedNonce = nnc,
		encryptedKeySecurityByte = (BS.pack . (: []) -> aad),
		encryptedCipherText = ct, encryptedMac = mac } = ec

type Ncryptsec = (SymKeyPrms, Encrypted)

data SymKeyPrms = SymKeyPrms {
	symKeyPrmsLogN :: Word8, symKeyPrmsSalt :: BS.ByteString }
	deriving Show

data Encrypted = Encrypted {
	encryptedVersion :: Word8, encryptedNonce :: BS.ByteString,
	encryptedKeySecurityByte :: Word8, encryptedCipherText :: BS.ByteString,
	encryptedMac :: BS.ByteString }
	deriving Show

encode :: SymKeyPrms -> Encrypted -> BS.ByteString
encode skp ec =
	encryptedVersion ec `BS.cons` symKeyPrmsLogN skp `BS.cons`
	symKeyPrmsSalt skp <> encryptedNonce ec <>
	(encryptedKeySecurityByte ec `BS.cons` encryptedCipherText ec) <>
	encryptedMac ec

decode :: MonadFail m => BS.ByteString -> m (SymKeyPrms, Encrypted)
decode bs = do
	[	[vsn], [lgn], BS.pack -> slt, BS.pack -> nnc,
		[ks], BS.pack -> ct, BS.pack -> mac ] <-
		pure $ BS.unpack bs `sep` [1, 1, 16, 24, 1, 32, 16]
	pure (	SymKeyPrms { symKeyPrmsLogN = lgn, symKeyPrmsSalt = slt },
		Encrypted {
			encryptedVersion = vsn, encryptedNonce = nnc,
			encryptedKeySecurityByte = ks, encryptedCipherText = ct,
			encryptedMac = mac } )
	where sep xs = \case
		[] -> []
		n : ns -> uncurry (:) . ((`sep` ns) `second`) $ splitAt n xs
