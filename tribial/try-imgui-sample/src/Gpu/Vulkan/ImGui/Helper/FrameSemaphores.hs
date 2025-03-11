{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.Helper.FrameSemaphores (
	FC(..), fCToMiddle, fCFromMiddle, FCListFromMiddle(..), fCListFromMiddle' ) where

import Data.Kind
import Data.TypeLevel.Tuple.Uncurry
import Data.HeteroParList (pattern (:**))
import Data.HeteroParList qualified as HPList

import Text.Show.ToolsYj

import Gpu.Vulkan.Semaphore.Internal qualified as Vk.Smph

import Gpu.Vulkan.ImGui.Helper.FrameSemaphores.Middle qualified as M

data FC sias srcs = FC {
	fCImageAcquiredSemaphore :: Vk.Smph.S sias,
	fCRenderCompleteSemaphore :: Vk.Smph.S srcs }
	deriving Show

instance ShowIO (U2 FC frsmas) where
	showIO (U2 f) = do
		sf <- showIO f
		pure $ "(U2 " ++ sf ++ ")"

fCToMiddle :: FC sias srcs -> M.FC
fCToMiddle FC {
	fCImageAcquiredSemaphore = Vk.Smph.S ias,
	fCRenderCompleteSemaphore = Vk.Smph.S rcs } = M.FC {
	M.fCImageAcquiredSemaphore = ias,
	M.fCRenderCompleteSemaphore = rcs }

fCFromMiddle :: M.FC -> FC sias srcs
fCFromMiddle M.FC {
	M.fCImageAcquiredSemaphore = ias,
	M.fCRenderCompleteSemaphore = rcs } = FC {
	fCImageAcquiredSemaphore = Vk.Smph.S ias,
	fCRenderCompleteSemaphore = Vk.Smph.S rcs }

class FCListFromMiddle (sss :: [(Type, Type)]) where
	fCListFromMiddle :: [M.FC] -> HPList.PL (U2 FC) sss

instance FCListFromMiddle '[] where
	fCListFromMiddle [] = HPList.Nil
	fCListFromMiddle _ = error "bad"

instance FCListFromMiddle sss => FCListFromMiddle ('(s1, s2) ': sss) where
	fCListFromMiddle (f : fs) = U2 (fCFromMiddle f) :** fCListFromMiddle fs
	fCListFromMiddle _ = error "bad"

fCListFromMiddle' :: [M.FC] -> (forall sss . HPList.PL (U2 FC) sss -> a) -> a
fCListFromMiddle' [] g = g HPList.Nil
fCListFromMiddle' (f : fs) g = fCListFromMiddle' fs \fs' -> g $ U2 (fCFromMiddle f) :** fs'
