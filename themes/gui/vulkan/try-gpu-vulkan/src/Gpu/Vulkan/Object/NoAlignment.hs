{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Object.NoAlignment (
	Atom, List, Image,
	AtomNoName, ListNoName, ImageNoName ) where

import Gpu.Vulkan.Object qualified as Vk.Obj

type Atom v nm = Vk.Obj.AtomNew 1 v nm
type List v nm = Vk.Obj.List 1 v nm
type Image v nm = Vk.Obj.Image 1 v nm

type AtomNoName v = Vk.Obj.AtomNoName 1 v
type ListNoName v = Vk.Obj.ListNoName 1 v
type ImageNoName v = Vk.Obj.ImageNoName 1 v
