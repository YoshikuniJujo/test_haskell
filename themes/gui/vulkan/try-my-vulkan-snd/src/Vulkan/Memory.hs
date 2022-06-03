{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Memory where

import Data.Word

newtype TypeIndex = TypeIndex Word32 deriving (Show, Enum, Num)
