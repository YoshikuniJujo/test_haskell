{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Image.Immutable where

import Data.Vector qualified as V
import Data.Word

data Gray = Gray {
	grayWidth :: Int, grayHeight :: Int, grayBody :: V.Vector Word8 }

grayRead :: Gray -> Int -> Int -> Word8
grayRead Gray { grayWidth = w, grayHeight = _h, grayBody = bd } x y =
	bd V.! (y * w + x)

grayUnconsRow :: Gray -> Maybe (V.Vector Word8, Gray)
grayUnconsRow Gray { grayHeight = 0 } = Nothing
grayUnconsRow Gray { grayWidth = w, grayHeight = h, grayBody = bd } = Just (
	V.take w bd,
	Gray { grayWidth = w, grayHeight = h - 1, grayBody = V.drop w bd } )

grayUnsnocRow :: Gray -> Maybe (Gray, V.Vector Word8)
grayUnsnocRow Gray { grayHeight = 0 } = Nothing
grayUnsnocRow Gray { grayWidth = w, grayHeight = h, grayBody = bd } = Just (
	Gray { grayWidth = w, grayHeight = h - 1, grayBody = dropR w bd },
	takeR w bd )

grayUnconsCol :: Gray -> Maybe (V.Vector Word8, Gray)
grayUnconsCol Gray { grayWidth = 0 } = Nothing
grayUnconsCol g@Gray { grayWidth = w, grayHeight = h } = Just (
	V.fromList (V.head <$> rs),
	Gray {	grayWidth = w - 1, grayHeight = h,
		grayBody = V.concat (V.tail <$> rs) } )
	where rs = rows g

grayUnsnocCol :: Gray -> Maybe (Gray, V.Vector Word8)
grayUnsnocCol Gray { grayWidth = 0 } = Nothing
grayUnsnocCol g@Gray { grayWidth = w, grayHeight = h } = Just (
	Gray {	grayWidth = w - 1, grayHeight = h,
		grayBody = V.concat (V.init <$> rs) },
	V.fromList (V.last <$> rs) )
	where rs = rows g

rows :: Gray -> [V.Vector Word8]
rows Gray { grayWidth = w, grayHeight = _h, grayBody = bd } = go bd
	where
	go :: V.Vector Word8 -> [V.Vector Word8]
	go v	| V.null v = []
		| otherwise = V.take w v : go (V.drop w v)

takeR :: Int -> V.Vector a -> V.Vector a
takeR n v = V.drop (V.length v - n) v

dropR :: Int -> V.Vector a -> V.Vector a
dropR n v = V.take (V.length v - n) v

fromAscii :: [String] -> Gray
fromAscii = fromList . ((asciiToWord8 <$>) <$>)

toAscii :: Gray -> [String]
toAscii g = (word8ToAscii <$>) . V.toList <$> rs
	where
	rs = rows g

printAsAscii :: Gray -> IO ()
printAsAscii = (putStrLn `mapM_`) . toAscii

asciiToWord8 :: Char -> Word8
asciiToWord8 = \case
	'.' -> 0; ',' -> 51; '-' -> 102; '!' -> 153; '+' -> 204; '*' -> 255
	_ -> error "bad"

word8ToAscii :: Word8 -> Char
word8ToAscii w
	| w < 26 = '.' | w < 77 = ',' | w < 128 = '-'
	| w < 179 = '!' | w < 230 = '+' | otherwise = '*'

fromList :: [[Word8]] -> Gray
fromList wss = Gray {
	grayWidth = w, grayHeight = h, grayBody = V.fromList $ concat wss }
	where
	w = length $ head wss
	h = length wss

sampleGray0, sampleGray1 :: Gray
sampleGray0 = fromAscii sampleAscii0
sampleGray1 = fromAscii sampleAscii1

sampleAscii0 :: [String]
sampleAscii0 = [
	"........................",
	"........********........",
	"........********........",
	"........********........",
	"........********........",
	"........********........",
	"........********........",
	"........********........",
	"........................" ]

sampleAscii1 :: [String]
sampleAscii1 = [
	"........................",
	"........********........",
	"........********........",
	"........**...***........",
	"........**...***........",
	"........**...***........",
	"........********........",
	"........********........",
	"........................" ]

filled :: Int -> Int -> Word8 -> Gray
filled w h c = Gray {
	grayWidth = w,
	grayHeight = h,
	grayBody = V.fromList $ replicate (w * h) c }
