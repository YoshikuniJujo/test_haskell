{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TypeLevel.List where

import Prelude hiding (length)

import Data.Kind

type family MapFst (zl :: [(Type, Type)]) where
	MapFst '[] = '[]
	MapFst ('(x, y) ': xys) = x ': MapFst xys

type family Zip (xs :: [Type]) (ys :: [Type]) where
	Zip '[] ys = '[]
	Zip xs '[] = '[]
	Zip (x ': xs) (y ': ys) = '(x, y) ': Zip xs ys

type family MapFst' (zl :: [Type]) where
	MapFst' '[] = '[]
	MapFst' ((x, y) ': xys) = x ': MapFst' xys

type family MapAddSnd (t :: Type) (l :: [Type]) = r | r -> l  where
	MapAddSnd t '[] = '[]
	MapAddSnd t (x ': xs) = (x, t) ': MapAddSnd t xs

class PrefixOf (xs :: [Type]) (ys :: [Type])

instance PrefixOf '[] ys

instance PrefixOf xs ys => PrefixOf (x ': xs) (x ': ys)

class InfixIndex (xs :: [Type]) (ys :: [Type]) where infixIndex :: Int

instance PrefixOf (x ': xs) (x ': ys) => InfixIndex (x ': xs) (x ': ys) where
	infixIndex = 0

instance {-# OVERLAPPABLE #-} InfixIndex xs ys => InfixIndex xs (y ': ys) where
	infixIndex = infixIndex @xs @ys + 1

class Length k (ts :: [k]) where length :: Int

instance Length k '[] where length = 0

instance Length k ts => Length k (t ': ts) where
	length = 1 + length @k @ts
