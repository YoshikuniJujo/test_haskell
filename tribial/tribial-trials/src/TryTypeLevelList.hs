{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryTypeLevelList where

import Data.Kind

class PrefixOf (xs :: [Type]) (ys :: [Type])

instance PrefixOf '[] ys

instance PrefixOf xs ys => PrefixOf (x ': xs) (x ': ys)

class InfixIndex (xs :: [Type]) (ys :: [Type]) where infixIndex :: Int

instance PrefixOf (x ': xs) (x ': ys) => InfixIndex (x ': xs) (x ': ys) where
	infixIndex = 0

instance {-# OVERLAPPABLE #-} InfixIndex xs ys => InfixIndex xs (y ': ys) where
	infixIndex = infixIndex @xs @ys + 1
