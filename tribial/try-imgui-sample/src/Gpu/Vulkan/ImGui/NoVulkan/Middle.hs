{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.NoVulkan.Middle (C.newFrame, begin, C.render) where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.C.String
import Control.Exception
import Data.Bool
import Data.Word

import Gpu.Vulkan.ImGui.NoVulkan.Enum
import Gpu.Vulkan.ImGui.NoVulkan.Core qualified as C

begin :: MaybeBool b =>
	String -> b -> WindowFlags -> (Bool -> IO a) -> IO (a, b)
begin nm opn (WindowFlags flgs) act = withCString nm \cnm ->
	maybeBool opn \popn ->
		bracket ((/= 0) <$> C.begin cnm popn flgs) (const C.end) act

class MaybeBool mb where maybeBool :: mb -> (Ptr Word8 -> IO a) -> IO (a, mb)

instance MaybeBool () where maybeBool () f = (, ()) <$> f nullPtr

instance MaybeBool Bool where
	maybeBool b f = alloca \pb ->
		poke pb (bool 0 1 b) >> (,) <$> f pb <*> ((/= 0) <$> peek pb)
