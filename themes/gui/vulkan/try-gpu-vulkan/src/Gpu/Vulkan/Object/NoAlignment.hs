{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Object.NoAlignment where

import Gpu.Vulkan.Object qualified as Vk.Obj

type Atom v mnm = Vk.Obj.Atom 1 v mnm
