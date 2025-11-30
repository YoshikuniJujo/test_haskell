{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.Png.Palette (
	Palette(..),
	encodePalette, elemIndexPalette, lookupPalette,

	readPalette, readPalette2, palette0, palette0',
	palette2ToPalette
	) where

import Data.Foldable
import Data.MonoTraversable
import Data.Vector qualified as V
import Data.Word
import Data.ByteString.FingerTree qualified as BSF
import Data.Color

import Data.Sequence qualified as Seq

data Palette = Palette (V.Vector (Word8, Word8, Word8)) deriving Show

data Palette2 = Palette2 (Seq.Seq (Word8, Word8, Word8)) deriving Show

snoc :: Palette2 -> Word8 -> Word8 -> Word8 -> Palette2
snoc (Palette2 p) r g b = Palette2 $ p Seq.:|> (r, g, b)

palette0 :: Palette
palette0 = Palette V.empty

palette0' :: Palette2
palette0' = Palette2 Seq.empty

readPalette :: BSF.ByteString -> Palette
readPalette = Palette . V.unfoldr \bs -> case BSF.splitAt' 3 bs of
	Nothing -> Nothing
	Just (otoList -> [r, g, b], bs') -> Just ((r, g, b), bs')
	_ -> error "bad"

readPalette2 :: (Palette2, [Word8]) -> BSF.ByteString -> (Palette2, [Word8])
readPalette2 = BSF.foldl' readByte1

readByte1 :: (Palette2, [Word8]) -> Word8 -> (Palette2, [Word8])
readByte1 (p, []) r = (p, [r])
readByte1 (p, [r]) g = (p, [r, g])
readByte1 (p, [r, g]) b = (snoc p r g b, [])
readByte1 _ _ = error "bad"

encodePalette :: Palette -> BSF.ByteString
encodePalette (Palette v) = foldl' (\bs (r, g, b) -> bs <> BSF.pack [r, g, b]) BSF.empty v

lookupPalette :: (RealFrac d, Integral i) => Palette -> i -> Rgb d
lookupPalette (Palette v) i = let (r, g, b) = v V.! fromIntegral i in RgbWord8 r g b

elemIndexPalette :: (RealFrac d, Num i) => Palette -> Rgb d -> Maybe i
elemIndexPalette (Palette v) (RgbWord8 r g b) =
	fromIntegral <$> V.elemIndex (r, g, b) v

seqToVector :: Seq.Seq a -> V.Vector a
seqToVector = V.fromList . toList

palette2ToPalette :: Palette2 -> Palette
palette2ToPalette (Palette2 p) = Palette $ seqToVector p
