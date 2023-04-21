{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds, PolyKinds, ConstraintKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Foreign.Storable.HeteroList (

	-- * Size and Alignment

	SizableList(..), sizeAlignments, wholeSize,

	-- * Pokable

	PokableList(..),

	-- * WithPoked

	-- ** Plain

	WithPokedHeteroToListM, withPokedHeteroToListM, withPokedWithHeteroListM,
	WithPokedHeteroToListM', withPokedHeteroToListM',

	-- ** CPS

	WithPokedHeteroToListCpsM,
	withPokedHeteroToListCpsM, withPokedWithHeteroListCpsM,
	WithPokedHeteroToListCpsM',
	withPokedHeteroToListCpsM', withPokedWithHeteroListCpsM'

	) where

import Foreign.Ptr
import Foreign.Storable.PeekPoke
import Data.Kind
import qualified Data.HeteroParList as HeteroParList
import Data.HeteroParList (pattern (:**))

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

type WithPokedHeteroToListM = HeteroParList.ConstraintHeteroToListM WithPoked

withPokedHeteroToListM :: (
	WithPokedHeteroToListM ss, Applicative m ) =>
	(forall s . WithPoked s => t s -> m a) -> HeteroParList.PL t ss -> m [a]
withPokedHeteroToListM = HeteroParList.constraintHeteroToListM @WithPoked

type WithPokedHeteroToListM' = HeteroParList.ConstraintHeteroToListM' WithPoked

withPokedHeteroToListM' :: forall k t' t ss m a .
	(WithPokedHeteroToListM' t' ss, Applicative m) =>
	(forall (s :: k) . WithPoked (t' s) => t s -> m a) ->
	HeteroParList.PL t ss -> m [a]
withPokedHeteroToListM' = HeteroParList.constraintHeteroToListM' @_ @WithPoked @t'

withPokedWithHeteroListM :: (WithPokedHeteroToListM ss, Applicative m) =>
		HeteroParList.PL t ss ->
		(forall s . WithPoked s => t s -> m a) -> m [a]
withPokedWithHeteroListM xs f = withPokedHeteroToListM f xs

type WithPokedHeteroToListCpsM =
	HeteroParList.ConstraintHeteroToListCpsM WithPoked

withPokedHeteroToListCpsM :: WithPokedHeteroToListCpsM ns =>
	(forall s . WithPoked s => t s -> (a -> m b) -> m b) ->
	HeteroParList.PL t ns ->
	([a] -> m b) -> m b
withPokedHeteroToListCpsM = HeteroParList.constraintHeteroToListCpsM @WithPoked

withPokedWithHeteroListCpsM :: WithPokedHeteroToListCpsM ss =>
	HeteroParList.PL t ss ->
	(forall s . WithPoked s => t s -> (a -> m b) -> m b) ->
	([a] -> m b) -> m b
withPokedWithHeteroListCpsM f xs = withPokedHeteroToListCpsM xs f

type WithPokedHeteroToListCpsM' =
	HeteroParList.ConstraintHeteroToListCpsM' WithPoked

withPokedHeteroToListCpsM' :: forall k t' t ns a m b .
	WithPokedHeteroToListCpsM' t' ns =>
	(forall (s :: k) . WithPoked (t' s) => t s -> (a -> m b) -> m b) ->
	HeteroParList.PL t ns -> ([a] -> m b) -> m b
withPokedHeteroToListCpsM' =
	HeteroParList.constraintHeteroToListCpsM' @_ @WithPoked @t'

withPokedWithHeteroListCpsM' :: forall t' t ss a m b .
	WithPokedHeteroToListCpsM' t' ss =>
	HeteroParList.PL t ss ->
	(forall s . WithPoked (t' s) => t s -> (a -> m b) -> m b) ->
	([a] -> m b) -> m b
withPokedWithHeteroListCpsM' f xs = withPokedHeteroToListCpsM' @_ @t' xs f
