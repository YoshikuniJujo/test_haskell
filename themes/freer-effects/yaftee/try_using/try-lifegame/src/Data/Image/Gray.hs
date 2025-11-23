{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Image.Gray (
	G(..), filled,
	grayRead,
	grayUnconsRow, grayUnsnocRow,
	grayUnconsCol, grayUnsnocCol,

	fromAscii, printAsAscii,
	sampleGray0, sampleGray1

	) where

import Data.Vector qualified as V
import Data.Word

data G = G {
	grayWidth :: Int, grayHeight :: Int, grayBody :: V.Vector Word8 }

grayRead :: G -> Int -> Int -> Word8
grayRead G { grayWidth = w, grayHeight = _h, grayBody = bd } x y =
	bd V.! (y * w + x)

grayUnconsRow :: G -> Maybe (V.Vector Word8, G)
grayUnconsRow G { grayHeight = 0 } = Nothing
grayUnconsRow G { grayWidth = w, grayHeight = h, grayBody = bd } = Just (
	V.take w bd,
	G { grayWidth = w, grayHeight = h - 1, grayBody = V.drop w bd } )

grayUnsnocRow :: G -> Maybe (G, V.Vector Word8)
grayUnsnocRow G { grayHeight = 0 } = Nothing
grayUnsnocRow G { grayWidth = w, grayHeight = h, grayBody = bd } = Just (
	G { grayWidth = w, grayHeight = h - 1, grayBody = dropR w bd },
	takeR w bd )

grayUnconsCol :: G -> Maybe (V.Vector Word8, G)
grayUnconsCol G { grayWidth = 0 } = Nothing
grayUnconsCol g@G { grayWidth = w, grayHeight = h } = Just (
	V.fromList (V.head <$> rs),
	G {	grayWidth = w - 1, grayHeight = h,
		grayBody = V.concat (V.tail <$> rs) } )
	where rs = rows g

grayUnsnocCol :: G -> Maybe (G, V.Vector Word8)
grayUnsnocCol G { grayWidth = 0 } = Nothing
grayUnsnocCol g@G { grayWidth = w, grayHeight = h } = Just (
	G {	grayWidth = w - 1, grayHeight = h,
		grayBody = V.concat (V.init <$> rs) },
	V.fromList (V.last <$> rs) )
	where rs = rows g

rows :: G -> [V.Vector Word8]
rows G { grayWidth = w, grayHeight = _h, grayBody = bd } = go bd
	where
	go :: V.Vector Word8 -> [V.Vector Word8]
	go v	| V.null v = []
		| otherwise = V.take w v : go (V.drop w v)

takeR :: Int -> V.Vector a -> V.Vector a
takeR n v = V.drop (V.length v - n) v

dropR :: Int -> V.Vector a -> V.Vector a
dropR n v = V.take (V.length v - n) v

fromAscii :: [String] -> G
fromAscii = fromList . ((asciiToWord8 <$>) <$>)

toAscii :: G -> [String]
toAscii g = (word8ToAscii <$>) . V.toList <$> rs
	where
	rs = rows g

printAsAscii :: G -> IO ()
printAsAscii = (putStrLn `mapM_`) . toAscii

asciiToWord8 :: Char -> Word8
asciiToWord8 = \case
	'.' -> 0; ',' -> 51; '-' -> 102; '!' -> 153; '+' -> 204; '*' -> 255
	_ -> error "bad"

word8ToAscii :: Word8 -> Char
word8ToAscii w
	| w < 26 = '.' | w < 77 = ',' | w < 128 = '-'
	| w < 179 = '!' | w < 230 = '+' | otherwise = '*'

fromList :: [[Word8]] -> G
fromList wss = G {
	grayWidth = w, grayHeight = h, grayBody = V.fromList $ concat wss }
	where
	w = length $ head wss
	h = length wss

sampleGray0, sampleGray1 :: G
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

filled :: Int -> Int -> Word8 -> G
filled w h c = G {
	grayWidth = w,
	grayHeight = h,
	grayBody = V.fromList $ replicate (w * h) c }
