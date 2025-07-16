{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.Png.Palette where

import Data.MonoTraversable
import Data.Vector qualified as V
import Data.Word
import Data.ByteString.FingerTree qualified as BSF
import Data.Color

data Palette = Palette (V.Vector (Word8, Word8, Word8)) deriving Show

palette0 :: Palette
palette0 = Palette $ V.empty

readPalette :: BSF.ByteString -> Palette
readPalette = Palette . V.unfoldr \bs -> case BSF.splitAt' 3 bs of
	Nothing -> Nothing
	Just (otoList -> [r, g, b], bs') -> Just ((r, g, b), bs')
	_ -> error "bad"

-- readingPalette :: BSF.ByteString -> (Palette, BSF.ByteString)
-- readingPalette

-- readByte1 :: (Palette, [Word8]) -> Word8 -> (Palette, [Word8])
-- readByte1

splitAt' :: Int -> V.Vector a -> Maybe (V.Vector a, V.Vector a)
splitAt' n v
	| n <= V.length v = Just $ V.splitAt n v
	| otherwise = Nothing

encodePalette :: Palette -> BSF.ByteString
encodePalette (Palette v) = foldl' (\bs (r, g, b) -> bs <> BSF.pack [r, g, b]) BSF.empty v

lookupPalette :: (RealFrac d, Integral i) => Palette -> i -> Rgb d
lookupPalette (Palette v) i = let (r, g, b) = v V.! fromIntegral i in RgbWord8 r g b

elemIndexPalette :: (RealFrac d, Num i) => Palette -> Rgb d -> Maybe i
elemIndexPalette (Palette v) (RgbWord8 r g b) =
	fromIntegral <$> V.elemIndex (r, g, b) v
