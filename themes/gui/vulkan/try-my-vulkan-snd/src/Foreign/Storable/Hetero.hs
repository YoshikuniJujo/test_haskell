{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Foreign.Storable.Hetero where

import Foreign.Ptr
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Data.Kind
import Data.HeteroList

class SizableList (vs :: [Type]) where
	sizes :: [Int]
	alignments :: [Int]

instance SizableList '[] where sizes = []; alignments = []

instance (Sizable t, SizableList ts) => SizableList (t ': ts) where
	sizes = sizeOf' @t: sizes @ts
	alignments = alignment' @t : alignments @ts

sizeAlignments' :: forall ts . SizableList ts => [(Int, Int)]
sizeAlignments' = zip (sizes @ts) (alignments @ts)

class StoreHetero' (ts :: [Type]) where
	storeHetero' :: Ptr () -> HeteroList' ts -> IO ()

instance StoreHetero' '[] where
	storeHetero' _ HVNil = pure ()

instance (Storable t, StoreHetero' ts) => StoreHetero' (t ': ts) where
	storeHetero' p (Id x :...: xs) = do
		poke (castPtr p') x
		storeHetero' (p' `plusPtr` sizeOf x) xs
		where p' = alignPtr p $ alignment x

storeHeteroSize :: forall ts . SizableList ts => HeteroList' ts -> Int
storeHeteroSize _ = calcSize 0 $ sizeAlignments' @ts

calcSize :: Int -> [(Int, Int)] -> Int
calcSize n [] = n
calcSize n ((sz, al) : szals) = calcSize (((n - 1) `div` al + 1) * al + sz) szals
