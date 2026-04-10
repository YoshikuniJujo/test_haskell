{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll, ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module WriteMonoral16 (

putMonoral16, putFloatList

) where

import Data.Word
import Data.ByteString qualified as BS


import Waveform
import Poppable

putFloatList :: FilePath -> [Float] -> IO ()
putFloatList fp = putMonoral16 fp . floatListToMonoral16

putMonoral16 :: FilePath -> Monoral16 -> IO ()
putMonoral16 fp = BS.writeFile fp . encodeMonoral16

encodeMonoral16 :: Monoral16 -> BS.ByteString
encodeMonoral16 (Monoral16 fmt dt) = addRiff
	$	"WAVE" <> fromChunk (toByteString <$> Chunk "fmt " fmt) <>
		fromChunk (Chunk "data" . BS.concat $ toByteString <$> dt)

addRiff :: BS.ByteString -> BS.ByteString
addRiff bs = "RIFF" <> toByteString @Word32 (fromIntegral $ BS.length bs) <> bs

fromChunk :: Chunk BS.ByteString -> BS.ByteString
fromChunk Chunk { chunkFourCC = fcc, chunkPayload = pld } =
	fcc <> toByteString @Word32 (fromIntegral sz) <> pld
	where sz = BS.length pld

data Chunk a = Chunk {
	chunkFourCC :: BS.ByteString,
	chunkPayload :: a }
	deriving Show

instance Functor Chunk where
	f `fmap` Chunk { chunkFourCC = fcc, chunkPayload = x } = Chunk {
		chunkFourCC = fcc,
		chunkPayload = f x }
