{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ClassGeneralK where

import GHC.TypeNats

class IsNatural n where natural :: Natural

instance IsNatural 0 where natural = 0

instance {-# OVERLAPPABLE #-} IsNatural (n - 1) => IsNatural n where
	natural = natural @(n - 1) + 1

instance IsNatural '[] where natural = 0
instance IsNatural n => IsNatural ('() ': n) where natural = natural @n + 1

sample1, sample2 :: Natural
sample1 = natural @15
sample2 = natural @'[ '(), '(), '(), '(), '()]
