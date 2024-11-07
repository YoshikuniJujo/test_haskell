{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE KindSignatures, DataKinds, ConstraintKinds, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.VertexInputState.Data.Type.TypeValMap (MapTypeVal2(..), MapTypeValMaybe2(..)) where

import Data.Kind

class MapTypeVal2 c (as :: [Type]) where
	mapTypeVal2 :: (forall a . c a => a -> b) -> [b]

instance MapTypeVal2 c '[] where mapTypeVal2 _ = []

instance (c a, MapTypeVal2 c as) => MapTypeVal2 c (a ': as) where
	mapTypeVal2 x = x (undefined :: a) : mapTypeVal2 @c @as x

class MapTypeValMaybe2 c (mas :: Maybe [Type]) where
	mapTypeValMaybe2 :: (forall a . c a => a -> b) -> Maybe [b]

instance MapTypeValMaybe2 c 'Nothing where mapTypeValMaybe2 _ = Nothing

instance MapTypeVal2 c as => MapTypeValMaybe2 c ('Just as) where
	mapTypeValMaybe2 x = Just $ mapTypeVal2 @c @as x
