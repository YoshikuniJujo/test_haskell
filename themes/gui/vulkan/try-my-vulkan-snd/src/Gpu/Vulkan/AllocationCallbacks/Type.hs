{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.AllocationCallbacks.Type where

import Data.Kind
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.TypeLevel.Uncurry

import Gpu.Vulkan.AllocationCallbacks.Middle qualified as M

newtype A s a = A { toMiddle :: M.A a } deriving Show

class ToMiddle' msa where
	type Snd msa :: Maybe Type
	toMiddle' :: TPMaybe.M (U2 A) msa -> TPMaybe.M M.A (Snd msa)

instance ToMiddle' 'Nothing where
	type Snd 'Nothing = 'Nothing
	toMiddle' TPMaybe.N = TPMaybe.N

instance ToMiddle' ('Just '(s, a)) where
	type Snd ('Just '(s, a)) = 'Just a
	toMiddle' (TPMaybe.J (U2 a)) = TPMaybe.J $ toMiddle a
