{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.TypeLevel.Maybe (M, pattern N, pattern J, Id(..)) where

import Foreign.Storable
import Data.TypeLevel.ParMaybe qualified as P

newtype Id a = Id a deriving (Show, Eq, Ord, Storable)

type M = P.M Id

pattern N :: M 'Nothing
pattern N <- P.N where N = P.N

pattern J :: a -> M ('Just a)
pattern J x <- (P.J (Id x)) where J x = P.J $ Id x
