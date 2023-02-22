{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Foreign.Storable.HeteroList (

	-- * Size and Alignment

	SizableList(..), sizeAlignments, wholeSize,

	-- * Pokable

	PokableList(..),

	-- * WithPoked

	-- ** Plain

	WithPokedHeteroToListM(..), withPokedWithHeteroListM,

	-- ** CPS

	WithPokedHeteroToListCpsM(..), withPokedWithHeteroListCpsM

	) where

import Foreign.Ptr
import Foreign.Storable.PeekPoke
import Data.Kind
import qualified Data.HeteroParList as HeteroParList
import Data.HeteroParList (pattern (:*), pattern (:**))

-- Size and Alignment

class SizableList (as :: [Type]) where sizes :: [Int]; alignments :: [Int]

instance SizableList '[] where sizes = []; alignments = []

instance (Sizable a, SizableList as) => SizableList (a ': as) where
	sizes = sizeOf' @a: sizes @as
	alignments = alignment' @a : alignments @as

sizeAlignments :: forall as . SizableList as => [(Int, Int)]
sizeAlignments = zip (sizes @as) (alignments @as)

wholeSize :: forall as . SizableList as => Int
wholeSize = calcSize 0 $ sizeAlignments @as

calcSize :: Int -> [(Int, Int)] -> Int
calcSize n [] = n
calcSize n ((sz, al) : szals) = calcSize (((n - 1) `div` al + 1) * al + sz) szals

-- Pokable

class SizableList as => PokableList (as :: [Type]) where
	pokeList :: Ptr x -> HeteroParList.L as -> IO ()

instance PokableList '[] where
	pokeList _ HeteroParList.Nil = pure ()

instance (Pokable a, PokableList as) => PokableList (a ': as) where
	pokeList ((`alignPtr` alignment' @a) -> p) (HeteroParList.Id x :** xs) = do
		poke' (castPtr p) x
		pokeList (p `plusPtr` sizeOf' @a) xs

-- WithPoked

class WithPokedHeteroToListM ss where
	withPokedHeteroToListM :: Applicative m =>
		(forall s . WithPoked s => t s -> m a) ->
		HeteroParList.PL t ss -> m [a]

instance WithPokedHeteroToListM '[] where
	withPokedHeteroToListM _ HeteroParList.Nil = pure []

instance (WithPoked s, WithPokedHeteroToListM ss) =>
	WithPokedHeteroToListM (s ': ss) where
	withPokedHeteroToListM f (x :** xs) =
		(:) <$> f x <*> withPokedHeteroToListM f xs

withPokedWithHeteroListM :: (WithPokedHeteroToListM ss, Applicative m) =>
		HeteroParList.PL t ss ->
		(forall s . WithPoked s => t s -> m a) -> m [a]
withPokedWithHeteroListM xs f = withPokedHeteroToListM f xs

class WithPokedHeteroToListCpsM ns where
	withPokedHeteroToListCpsM ::
		(forall s . WithPoked s => t s -> (a -> m b) -> m b) ->
		HeteroParList.PL t ns ->
		([a] -> m b) -> m b

instance WithPokedHeteroToListCpsM '[] where
	withPokedHeteroToListCpsM _ HeteroParList.Nil g = g []

instance (WithPoked n, WithPokedHeteroToListCpsM ns) =>
	WithPokedHeteroToListCpsM (n ': ns) where
	withPokedHeteroToListCpsM f (x :** xs) g =
		f x \y -> withPokedHeteroToListCpsM f xs \ys -> g $ y : ys

withPokedWithHeteroListCpsM :: WithPokedHeteroToListCpsM ss =>
	HeteroParList.PL t ss ->
	(forall s . WithPoked s => t s -> (a -> m b) -> m b) ->
	([a] -> m b) -> m b
withPokedWithHeteroListCpsM f xs = withPokedHeteroToListCpsM xs f
