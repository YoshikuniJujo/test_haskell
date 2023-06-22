{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds, PolyKinds, ConstraintKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Foreign.Storable.HeteroList (

	-- * SIZE AND ALIGNMENT

	wholeSize, sizeAlignments, infixOffsetSize,
	SizeAlignmentList, InfixOffsetSize, PrefixSize,

	-- * POKABLE

	PokableList(..),

{-
	-- * WITHPOKED

	-- ** Plain

	WithPokedHeteroToListM, withPokedHeteroToListM, withPokedWithHeteroListM,
	WithPokedHeteroToListM', withPokedHeteroToListM',

	-- ** CPS

	WithPokedHeteroToListCpsM,
	withPokedHeteroToListCpsM, withPokedWithHeteroListCpsM,
	WithPokedHeteroToListCpsM',
	withPokedHeteroToListCpsM', withPokedWithHeteroListCpsM'
	-}

	) where

import Foreign.Ptr
import Foreign.Storable.PeekPoke
import Data.Kind
import qualified Data.HeteroParList as HeteroParList
import Data.HeteroParList (pattern (:**))

-- Size and Alignment

{-
class SizableList (as :: [Type]) where sizes :: [Int]; alignments :: [Int]

instance SizableList '[] where sizes = []; alignments = []

instance (Sizable a, SizableList as) => SizableList (a ': as) where
	sizes = sizeOf' @a: sizes @as
	alignments = alignment' @a : alignments @as
	-}

sizeAlignments :: forall as . SizeAlignmentList as => [(Int, Int)]
sizeAlignments = sizeAlignmentsFromSizeAlignmentList (sizeAlignmentList @as)
	-- zip (sizes @as) (alignments @as)

sizeAlignmentsFromSizeAlignmentList ::
	HeteroParList.PL SizeAlignmentOfType as -> [(Int, Int)]
sizeAlignmentsFromSizeAlignmentList HeteroParList.Nil = []
sizeAlignmentsFromSizeAlignmentList
	(SizeAlignmentOfType sz algn :** sas) =
	(sz, algn) : sizeAlignmentsFromSizeAlignmentList sas

wholeSize :: forall as . SizeAlignmentList as => Int
wholeSize = calcSize 0 $ sizeAlignments @as

calcSize :: Int -> [(Int, Int)] -> Int
calcSize n [] = n
calcSize n ((sz, al) : szals) = calcSize (((n - 1) `div` al + 1) * al + sz) szals

infixOffsetSize :: forall (part :: [Type]) (whole :: [Type]) .
	InfixOffsetSize part whole => (Offset, Size)
infixOffsetSize = infixOffsetSizeFromSizeAlignmentList @part @whole
	0 (sizeAlignmentList @whole)

data SizeAlignmentOfType (tp :: Type) = SizeAlignmentOfType Size Alignment
	deriving Show

type Size = Int; type Alignment = Int; type Offset = Int

class SizeAlignmentList ts where
	sizeAlignmentList :: HeteroParList.PL SizeAlignmentOfType ts

instance SizeAlignmentList '[] where sizeAlignmentList = HeteroParList.Nil

instance (Sizable t, SizeAlignmentList ts) => SizeAlignmentList (t ': ts) where
	sizeAlignmentList = SizeAlignmentOfType (sizeOf' @t) (alignment' @t) :**
		sizeAlignmentList @ts

class SizeAlignmentList whole => InfixOffsetSize (part :: [Type]) whole where
	infixOffsetSizeFromSizeAlignmentList :: Size ->
		HeteroParList.PL SizeAlignmentOfType whole -> (Offset, Size)

instance (
	Sizable t, SizeAlignmentList whole,
	(t ': ts) `PrefixSize` (t ': whole) ) =>
	InfixOffsetSize (t ': ts) (t ': whole) where
	infixOffsetSizeFromSizeAlignmentList sz0
		saa@(SizeAlignmentOfType _ algn :** _) = (
			align algn sz0,
			prefixSizeFromSizeAlignmentList @(t ': ts) 0 saa )

instance {-# OVERLAPPABLE #-} (Sizable t, InfixOffsetSize ts whole) =>
	InfixOffsetSize ts (t ': whole) where
	infixOffsetSizeFromSizeAlignmentList sz0
		(SizeAlignmentOfType sz algn :** sas) =
		infixOffsetSizeFromSizeAlignmentList @ts
			(align algn sz0 + sz) sas

class PrefixSize (part :: [Type]) whole where
	prefixSizeFromSizeAlignmentList :: Size ->
		HeteroParList.PL SizeAlignmentOfType whole -> Size

instance PrefixSize '[] whole where prefixSizeFromSizeAlignmentList sz _ = sz

instance PrefixSize ts whole => PrefixSize (t ': ts) (t ': whole) where
	prefixSizeFromSizeAlignmentList sz0
		(SizeAlignmentOfType sz algn :** sas) =
		prefixSizeFromSizeAlignmentList @ts (align algn sz0 + sz) sas

align :: Alignment -> Size -> Offset
align algn sz = ((sz - 1) `div` algn + 1) * algn

-- Pokable

class SizeAlignmentList as => PokableList (as :: [Type]) where
	pokeList :: Ptr x -> HeteroParList.L as -> IO ()

instance PokableList '[] where
	pokeList _ HeteroParList.Nil = pure ()

instance (Pokable a, PokableList as) => PokableList (a ': as) where
	pokeList ((`alignPtr` alignment' @a) -> p) (HeteroParList.Id x :** xs) = do
		poke' (castPtr p) x
		pokeList (p `plusPtr` sizeOf' @a) xs

{-
-- WithPoked

type WithPokedHeteroToListM = HeteroParList.ToListWithCM WithPoked

withPokedHeteroToListM :: (
	WithPokedHeteroToListM ss, Applicative m ) =>
	(forall s . WithPoked s => t s -> m a) -> HeteroParList.PL t ss -> m [a]
withPokedHeteroToListM = HeteroParList.toListWithCM @WithPoked

type WithPokedHeteroToListM' = HeteroParList.ToListWithCM' WithPoked

withPokedHeteroToListM' :: forall k t' t ss m a .
	(WithPokedHeteroToListM' t' ss, Applicative m) =>
	(forall (s :: k) . WithPoked (t' s) => t s -> m a) ->
	HeteroParList.PL t ss -> m [a]
withPokedHeteroToListM' = HeteroParList.toListWithCM' @_ @_ @WithPoked @t'

withPokedWithHeteroListM :: (WithPokedHeteroToListM ss, Applicative m) =>
		HeteroParList.PL t ss ->
		(forall s . WithPoked s => t s -> m a) -> m [a]
withPokedWithHeteroListM xs f = withPokedHeteroToListM f xs

type WithPokedHeteroToListCpsM = HeteroParList.ToListWithCCpsM WithPoked

withPokedHeteroToListCpsM :: WithPokedHeteroToListCpsM ns =>
	(forall s . WithPoked s => t s -> (a -> m b) -> m b) ->
	HeteroParList.PL t ns ->
	([a] -> m b) -> m b
withPokedHeteroToListCpsM = HeteroParList.toListWithCCpsM @WithPoked

withPokedWithHeteroListCpsM :: WithPokedHeteroToListCpsM ss =>
	HeteroParList.PL t ss ->
	(forall s . WithPoked s => t s -> (a -> m b) -> m b) ->
	([a] -> m b) -> m b
withPokedWithHeteroListCpsM f xs = withPokedHeteroToListCpsM xs f

type WithPokedHeteroToListCpsM' =
	HeteroParList.ToListWithCCpsM' WithPoked

withPokedHeteroToListCpsM' :: forall k t' t ns a m b .
	WithPokedHeteroToListCpsM' t' ns =>
	(forall (s :: k) . WithPoked (t' s) => t s -> (a -> m b) -> m b) ->
	HeteroParList.PL t ns -> ([a] -> m b) -> m b
withPokedHeteroToListCpsM' =
	HeteroParList.toListWithCCpsM' @_ @WithPoked @t'

withPokedWithHeteroListCpsM' :: forall t' t ss a m b .
	WithPokedHeteroToListCpsM' t' ss =>
	HeteroParList.PL t ss ->
	(forall s . WithPoked (t' s) => t s -> (a -> m b) -> m b) ->
	([a] -> m b) -> m b
withPokedWithHeteroListCpsM' f xs = withPokedHeteroToListCpsM' @_ @t' xs f
-}
