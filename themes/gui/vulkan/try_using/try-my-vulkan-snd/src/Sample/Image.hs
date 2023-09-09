{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Sample.Image (ImageRgba8(..)) where

import GHC.Generics
import Foreign.Storable
import Data.MonoTraversable
import Data.List
import Data.Word

import qualified Foreign.Storable.Generic
import qualified Data.Sequences as Seq
import qualified Data.Vector.Storable as V

data Rgba8 = Rgba8 {
	rgba8Red :: Word8,
	rgba8Green :: Word8,
	rgba8Blue :: Word8,
	rgba8Alpha :: Word8 } deriving (Show, Generic)

instance Foreign.Storable.Generic.G Rgba8

instance Storable Rgba8 where
	sizeOf = Foreign.Storable.Generic.gSizeOf
	alignment = Foreign.Storable.Generic.gAlignment
	peek = Foreign.Storable.Generic.gPeek
	poke = Foreign.Storable.Generic.gPoke

data ImageRgba8 = ImageRgba8 (V.Vector Word8) deriving Show

imageRgba8ToList :: ImageRgba8 -> [Rgba8]
imageRgba8ToList (ImageRgba8 img) = irtl 0 img
	where
	ln = V.length img
	irtl i l
		| i < ln - 3 =
			Rgba8 (l V.! i) (l V.! (i + 1))
				(l V.! (i + 2)) (l V.! (i + 3)) : irtl (i + 4) l
		| otherwise = []

imageRgba8FromList :: [Rgba8] -> ImageRgba8
imageRgba8FromList =
	ImageRgba8 . V.fromList . concatMap \(Rgba8 r g b a) -> [r, g, b, a]

type instance Element ImageRgba8 = Rgba8 

instance MonoFunctor ImageRgba8 where
	omap f = imageRgba8FromList . map f . imageRgba8ToList

instance MonoFoldable ImageRgba8 where
	ofoldMap f = ofoldMap f . imageRgba8ToList
	ofoldr op x0 = ofoldr op x0 . imageRgba8ToList
	ofoldl' op x0 = ofoldl' op x0 . imageRgba8ToList
	ofoldr1Ex op = ofoldr1Ex op . imageRgba8ToList
	ofoldl1Ex' op = ofoldl1Ex' op . imageRgba8ToList

instance MonoTraversable ImageRgba8 where
	otraverse f = (imageRgba8FromList <$>) . traverse f . imageRgba8ToList

instance Semigroup ImageRgba8 where
	ImageRgba8 irs1 <> ImageRgba8 irs2 = ImageRgba8 $ irs1 <> irs2

instance Monoid ImageRgba8 where
	mempty = ImageRgba8 V.empty

instance MonoPointed ImageRgba8 where
	opoint = ImageRgba8 . V.fromList . \(Rgba8 r g b a) -> [r, g, b, a]

instance GrowingAppend ImageRgba8 where

instance Seq.SemiSequence ImageRgba8 where
	type Index ImageRgba8 = Int
	intersperse r =
		imageRgba8FromList . intersperse r . imageRgba8ToList
	reverse = imageRgba8FromList . reverse . imageRgba8ToList
	find p = find p . imageRgba8ToList
	sortBy cmp = imageRgba8FromList . sortBy cmp . imageRgba8ToList
	cons r = imageRgba8FromList . (r :) . imageRgba8ToList
	snoc img r = imageRgba8FromList . (++ [r]) $ imageRgba8ToList img

instance Seq.IsSequence ImageRgba8 where
	fromList = imageRgba8FromList
