{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.ColorBlendState where

import Foreign.Ptr
import Foreign.Marshal.Array
import Control.Monad.Cont
import Data.Foldable
import Data.List.Length

import Vulkan.Base
import Vulkan.LogicOp

import qualified Vulkan.Pipeline.ColorBlendState.Internal as I

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: I.CreateFlags,
	createInfoLogicOpEnable :: Bool,
	createInfoLogicOp :: LogicOp,
	createInfoAttachments :: [I.AttachmentState],
	createInfoBlendConstants :: LengthL 4 Float }
	deriving Show

createInfoToC :: Pointable n => CreateInfo n -> (I.CreateInfo -> IO a) -> IO a
createInfoToC CreateInfo {
	createInfoNext = mnxt, createInfoFlags = flgs,
	createInfoLogicOpEnable = (boolToBool32 -> loe),
	createInfoLogicOp = lo,
	createInfoAttachments = as,
	createInfoBlendConstants = (toList -> bcs) } = runContT do
	(castPtr -> pnxt) <- ContT $ withMaybePointer mnxt
	let	ac = length as
	pas <- ContT $ allocaArray ac
	lift $ pokeArray pas as
	pure I.CreateInfo {
		I.createInfoSType = (),
		I.createInfoPNext = pnxt,
		I.createInfoFlags = flgs,
		I.createInfoLogicOpEnable = loe,
		I.createInfoLogicOp = lo,
		I.createInfoAttachmentCount = fromIntegral ac,
		I.createInfoPAttachments = pas,
		I.createInfoBlendConstants = bcs }
