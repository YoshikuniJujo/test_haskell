{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.TypeLevel.Maybe (M(..)) where

import Data.Kind

data M (mt :: Maybe Type) where
	N :: M 'Nothing
	J :: a -> M ('Just a)

deriving instance Show (M 'Nothing)
deriving instance Show a => Show (M ('Just a))

deriving instance Eq (M 'Nothing)
deriving instance Eq a => Eq (M ('Just a))
