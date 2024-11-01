{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.TypeLevel.ParMaybe (M(..), Map, maybe, map, nil) where

import Prelude hiding (map, maybe)
import Data.Kind

data M (t :: k -> Type) (m :: Maybe k) where
	N :: M t 'Nothing
	J :: t a -> M t ('Just a)

deriving instance Show (M t 'Nothing)
deriving instance Show (t a) => Show (M t ('Just a))

deriving instance Eq (M t 'Nothing)
deriving instance Eq (t a) => Eq (M t ('Just a))

deriving instance Ord (M t 'Nothing)
deriving instance Ord (t a) => Ord (M t ('Just a))

type family Map (f :: k -> l) (m :: Maybe k) where
	Map _f 'Nothing = 'Nothing
	Map f ('Just x) = 'Just (f x)

maybe :: a -> (forall s . t s -> a) -> M t ms -> a
maybe d _ N = d
maybe _ f (J x) = f x

map :: (forall s . t s -> t' (f s)) -> M t ms -> M t' (Map f ms)
map _ N = N
map f (J x) = J $ f x

nil :: M (t :: k -> Type) 'Nothing
nil = N
