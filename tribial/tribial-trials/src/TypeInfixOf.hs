{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TypeInfixOf where

import Control.Arrow
import Data.Kind

class InfixIndex (ts :: [Type]) (ts' :: [Type]) where infixIndex :: Int

instance InfixIndex '[t] (t ': ts) where infixIndex = 0

instance {-# OVERLAPPABLE #-} InfixIndex ts ts' =>
	InfixIndex (t ': ts) (t ': ts') where
	infixIndex = 0

instance {-# OVERLAPPABLE #-} InfixIndex ts ts' =>
	InfixIndex ts (t ': ts') where
	infixIndex = 1 + infixIndex @ts @ts'

class Prefix (ts :: [Type]) (ts' :: [Type])

instance Prefix '[] ts

instance Prefix ts ts' => Prefix (t : ts) (t : ts')

class Some (ts :: [Type]) (tss :: [[Type]]) where some :: Int -> (Int, Int)

instance Prefix ts ts' => Some (t ': ts) ((t ': ts') : tss) where
	some _ = (0, 0)

instance {-# OVERLAPPABLE #-} Some ts (ts' : tss) =>
	Some ts ((t ': ts') ': tss) where
	some c = (+ 1) `second` (some @ts @(ts' : tss) $ c + 1)

instance {-# OVERLAPPABLE #-} Some ts tss =>
	Some ts ('[] ': tss) where
	some c = (a + 1, b - c) where (a, b) = some @ts @tss 0

first3 :: (a -> b) -> (a, c, d) -> (b, c, d)
first3 f (x, y, z) = (f x, y, z)

second3 :: (b -> c) -> (a, b, d) -> (a, c, d)
second3 f (x, y, z) = (x, f y, z)

third3 :: (c -> d) -> (a, b, c) -> (a, b, d)
third3 f (x, y, z) = (x, y, f z)

some' :: forall ts tss . Some ts tss => (Int, Int)
some' = some @ts @tss 0
