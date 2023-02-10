{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
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

	PokableList(..) ) where

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
