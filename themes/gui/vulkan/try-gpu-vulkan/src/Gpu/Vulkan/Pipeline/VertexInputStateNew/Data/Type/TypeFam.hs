{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.VertexInputStateNew.Data.Type.TypeFam where

import Data.Kind

type family FromJust (mx :: Maybe x) :: x where
	FromJust ('Just x) = x

type family AppMaybe (f :: x -> y) (mx :: Maybe x) :: Maybe y where
	AppMaybe _ 'Nothing = 'Nothing
	AppMaybe f ('Just x) = 'Just (f x)

type family Until t (ts :: [Type]) :: Maybe [Type] where
	Until _ '[] = 'Nothing
	Until t (t : ts) = 'Just '[t]
	Until t (t' : ts) = AppMaybe ((:) t') (Until t ts)
