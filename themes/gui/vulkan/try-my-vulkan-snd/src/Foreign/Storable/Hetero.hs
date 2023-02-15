{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Foreign.Storable.Hetero (

	-- * Size and Alignment

	SizableList(..), sizeAlignments, wholeSize,

	-- * Pokable

	PokableList(..),

	-- * WithPoked

	WithPokedToListM(..),
	WithPokedHeteroMap(..)

	) where

import Foreign.Ptr
import Foreign.Storable.PeekPoke
import Data.Kind
import Data.HeteroList

-- Size and Alignment

class SizableList (vs :: [Type]) where
	sizes :: [Int]
	alignments :: [Int]

instance SizableList '[] where sizes = []; alignments = []

instance (Sizable t, SizableList ts) => SizableList (t ': ts) where
	sizes = sizeOf' @t: sizes @ts
	alignments = alignment' @t : alignments @ts

sizeAlignments :: forall ts . SizableList ts => [(Int, Int)]
sizeAlignments = zip (sizes @ts) (alignments @ts)

wholeSize :: forall ts . SizableList ts => Int
wholeSize = calcSize 0 $ sizeAlignments @ts

calcSize :: Int -> [(Int, Int)] -> Int
calcSize n [] = n
calcSize n ((sz, al) : szals) = calcSize (((n - 1) `div` al + 1) * al + sz) szals

-- Pokable

class SizableList ts => PokableList (ts :: [Type]) where
	pokeList :: Ptr a -> HeteroList' ts -> IO ()

instance PokableList '[] where
	pokeList _ HVNil = pure ()

instance (Pokable t, PokableList ts) => PokableList (t ': ts) where
	pokeList ((`alignPtr` alignment' @t) -> p) (Id x :...: xs) = do
		poke' (castPtr p) x
		pokeList (p `plusPtr` sizeOf' @t) xs

-- WithPoked

class WithPokedToListM ns where
	withPokedToListM :: Monad m =>
		(forall n . WithPoked n => t n -> m t') -> HeteroVarList t ns -> m [t']

instance WithPokedToListM '[] where withPokedToListM _ HVNil = pure []

instance (WithPoked n, WithPokedToListM ns) => WithPokedToListM (n ': ns) where
	withPokedToListM f (x :...: xs) = (:) <$> f x <*> withPokedToListM f xs

class WithPokedHeteroMap ns where
	withPokedHeteroMapM :: HeteroVarList t ns ->
		(forall n . WithPoked n => t n -> (a -> m ()) -> m ()) ->
		([a] -> m ()) -> m ()

instance WithPokedHeteroMap '[] where
	withPokedHeteroMapM HVNil _ g = g []

instance (WithPoked n, WithPokedHeteroMap ns) =>
	WithPokedHeteroMap (n ': ns) where
	withPokedHeteroMapM (x :...: xs) f g =
		f x \y -> withPokedHeteroMapM xs f \ys -> g $ y : ys
