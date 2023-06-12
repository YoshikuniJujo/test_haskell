{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.TypeLevel.List where

import Prelude hiding (length)

class Length (as :: [k]) where length :: Integral n => n
instance Length '[] where length = 0
instance Length as => Length (a ': as) where length = length @_ @as + 1
