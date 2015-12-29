{-# LANGUAGE TupleSections, OverloadedStrings, PackageImports #-}

module IHDR (
	IHDR(..), ColorType(..), HasColor(..), HasAlpha(..), decode, encode
	) where

import "monads-tf" Control.Monad.State (evalStateT, replicateM)
import Data.Word (Word8)
import qualified Data.ByteString as BS (concat)

import Chunks (Chunk(..))
import Item (item, byte)
import Bits ((.|.), shiftL, popBits, beFromByteString, beToByteString)

data IHDR = IHDR {
	width :: Int, height :: Int, bitDepth :: Word8, colorType :: ColorType,
	compMethod :: Word8, filterMethod :: Word8, ilaceMethod :: Word8
	} deriving Show

data ColorType = PaletteColor | NoPalette HasColor HasAlpha deriving Show

data HasColor = Grayscale | HasColor deriving (Show, Enum)

data HasAlpha = NoAlpha | HasAlpha deriving (Show, Enum)

encode :: IHDR -> Chunk
encode h = Chunk { typ = "IHDR", dat = BS.concat [
	beToByteString 4 $ width h, beToByteString 4 $ height h,
	beToByteString 1 $ bitDepth h,
	beToByteString 1 (ct $ colorType h :: Word8),
	beToByteString 1 $ compMethod h,
	beToByteString 1 $ filterMethod h,
	beToByteString 1 $ ilaceMethod h ] }
	where
	ct PaletteColor = 3
	ct (NoPalette hc ha) =
		fromIntegral $ fromEnum hc `shiftL` 1 .|. fromEnum ha `shiftL` 2

decode :: Chunk -> Maybe IHDR
decode (Chunk "IHDR" bs) = (`evalStateT` bs) $ do
	[w, h] <- replicateM 2 . item 4 $ Just . beFromByteString
	d <- item 1 byte
	c <- item 1 $ (ct =<<) . byte
	[cm, f, i] <- replicateM 3 $ item 1 byte
	return $ IHDR w h d c cm f i
	where ct w = case w `popBits` 3 of
		([True, True, False], 0) -> Just PaletteColor
		([False, hc, ha], 0) -> Just $
			NoPalette (toEnum $ fromEnum hc) (toEnum $ fromEnum ha)
		_ -> Nothing
decode _ = Nothing
