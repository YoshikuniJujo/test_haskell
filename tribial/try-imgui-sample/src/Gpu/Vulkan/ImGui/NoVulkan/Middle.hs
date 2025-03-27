{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.NoVulkan.Middle (
	C.newFrame, begin, C.render,

	getDrawData, DrawData(..), drawDataDisplaySize ) where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.C.String
import Control.Exception
import Data.Word

import Gpu.Vulkan.ImGui.NoVulkan.Enum
import Gpu.Vulkan.ImGui.NoVulkan.Core qualified as C

begin :: MaybeBool b =>
	String -> WindowFlags -> (Bool -> IO a) -> IO (a, b)
begin nm (WindowFlags flgs) act = withCString nm \cnm ->
	maybeBool \popn ->
		bracket ((/= 0) <$> C.begin cnm popn flgs) (const C.end) act

class MaybeBool mb where maybeBool :: (Ptr Word8 -> IO a) -> IO (a, mb)

instance MaybeBool () where maybeBool f = (, ()) <$> f nullPtr

instance MaybeBool Bool where
	maybeBool f = alloca \pb -> (,) <$> f pb <*> ((/= 0) <$> peek pb)

newtype DrawData = DrawData C.DrawData deriving Show

getDrawData :: IO DrawData
getDrawData = DrawData <$> C.getDrawData

drawDataDisplaySize :: DrawData -> (Float, Float)
drawDataDisplaySize (DrawData dd) = C.drawDataDisplaySize dd
