{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GeneralFoldable where

import GHC.Base (build)
import Data.Word

import qualified Data.Vector.Unboxed as UV
import qualified Data.ByteString as BS

class GFoldable g where
	type Element g
	gfoldr :: (Element g -> b -> b) -> b -> g -> b
	gtoList :: g -> [Element g]

	gtoList g = build \c n -> gfoldr c n g

{-
newtype G g = G { unG :: g } deriving Show

instance Foldable t => GFoldable (G (t a)) where
	type Element (G (t a)) = a
	gfoldr op v = foldr op v . unG
	-}

instance {-# OVERLAPPABLE #-} Foldable t => GFoldable (t a) where
	type Element (t a) = a
	gfoldr = foldr

instance GFoldable BS.ByteString where
	type Element BS.ByteString = Word8
	gfoldr = BS.foldr

instance UV.Unbox a => GFoldable (UV.Vector a) where
	type Element (UV.Vector a) = a
	gfoldr = UV.foldr

{-
instance Foldable UV.Vector where
	foldr :: UV.Unbox a => (a -> b -> b) -> b -> UV.Vector a -> b
	foldr = UV.foldr
	-}

-- type Hack a a = UV.Vector a
