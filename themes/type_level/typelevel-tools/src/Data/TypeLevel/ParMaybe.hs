{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.TypeLevel.ParMaybe where

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
