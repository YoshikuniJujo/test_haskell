{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.TypeLevel.List (Length(..), IsPrefixOf, InfixIndex(..)) where

import Prelude hiding (length)

import Data.Kind

class Length (as :: [k]) where length :: Integral n => n
instance Length '[] where length = 0
instance Length as => Length (a ': as) where length = length @_ @as + 1

class (xs :: [k]) `IsPrefixOf` (ys :: [k])
instance '[] `IsPrefixOf` ys
instance xs `IsPrefixOf` ys => (x ': xs) `IsPrefixOf` (x ': ys)

class InfixIndex (xs :: [Type]) (ys :: [Type]) where infixIndex :: Int

instance (x ': xs) `IsPrefixOf` (x ': ys) =>
	InfixIndex (x ': xs) (x ': ys) where
	infixIndex = 0

instance {-# OVERLAPPABLE #-} InfixIndex xs ys => InfixIndex xs (y ': ys) where
	infixIndex = infixIndex @xs @ys + 1
