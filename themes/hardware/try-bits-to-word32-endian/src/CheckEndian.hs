{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CheckEndian (targetEndian) where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Data.Word
import System.Environment

data Endian = LittleEndian | BigEndian | UnknownEndian deriving Show

foo :: Word32 -> IO [Word8]
foo w32 = alloca \p -> do
	poke p w32
	peekArray 4 $ castPtr p :: IO [Word8]

checkEndian :: IO Endian
checkEndian = (<$> foo 0x01020304) \case
	[4, 3, 2, 1] -> LittleEndian
	[1, 2, 3, 4] -> BigEndian
	_ -> UnknownEndian

targetEndian :: IO (Either String Endian)
targetEndian = lookupEnv "GHC_TARGET_ENDIAN" >>= \case
	Just "little-endian" -> pure $ Right LittleEndian
	Just "big-endian" -> pure $ Right BigEndian
	Just edn -> pure . Left $ "no such endian: " ++ edn ++ "\n" ++
		"GHC_TARGET_ENDIAN: little-endian or big-endian"
	Nothing -> Right <$> checkEndian
