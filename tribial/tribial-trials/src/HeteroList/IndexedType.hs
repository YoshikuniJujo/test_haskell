{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGuAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module HeteroList.IndexedType where

import Data.Kind

infixr 5 :.

data HeteroList ts where
	Nil :: HeteroList '[]
	(:.) :: t -> HeteroList ts -> HeteroList (t ': ts)

class Index t (ts :: [Type]) where index :: Int

instance Index t (t ': ts) where index = 0

instance {-# OVERLAPPABLE #-} Index t ts => Index t (t' ': ts) where index = index @t @ts + 1

data Indexed t = forall ts . Index t ts => Indexed (HeteroList ts)

indexedToRaw :: forall t a . Indexed t -> (forall ts . (HeteroList ts, Int) -> a) -> a
indexedToRaw (Indexed (lst :: HeteroList ts)) f =
	f (lst, g lst)
	where
	g :: forall ts' . Index t ts' => HeteroList ts' -> Int
	g _ = index @t @ts
