{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE KindSignatures, DataKinds, ConstraintKinds, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Type.TypeValMap where

import Data.Kind

class MapTypeVal2 c (as :: [Type]) where
	mapTypeVal2 :: (forall a . c a => a -> b) -> [b]

instance MapTypeVal2 c '[] where mapTypeVal2 _ = []

instance (c a, MapTypeVal2 c as) => MapTypeVal2 c (a ': as) where
	mapTypeVal2 x = x (undefined :: a) : mapTypeVal2 @c @as x
