{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll, ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Poppable (Poppable(..), pop) where

import Control.Arrow
import Data.Bits
import Data.Word
import Data.Int
import Data.ByteString qualified as BS

pop :: forall a . Poppable a => BS.ByteString -> (a, BS.ByteString)
pop bs = fromByteString `first` (BS.splitAt (byteLength @a) bs)

class Poppable a where
	byteLength :: Int
	fromByteString :: BS.ByteString -> a

instance Poppable Word16 where
	byteLength = 2
	fromByteString bs = case fromIntegral <$> BS.unpack (BS.take 2 bs) of
		[a, b] -> a .|. b `shiftL` 8
		_ -> error "never occur"

instance Poppable Int16 where
	byteLength = 2
	fromByteString bs = case fromIntegral <$> BS.unpack (BS.take 2 bs) of
		[a, b] -> a .|. b `shiftL` 8
		_ -> error "never occur"

instance Poppable Word32 where
	byteLength = 4
	fromByteString =
		foldr (\x -> (fromIntegral x .|.) . (`shiftL` 8)) 0 . BS.unpack
