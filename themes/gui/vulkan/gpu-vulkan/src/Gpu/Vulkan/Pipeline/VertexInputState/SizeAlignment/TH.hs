{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.VertexInputState.SizeAlignment.TH (
	instanceSizeAlignmentListTuple,
	instanceSizeAlignmentListUntilTuple ) where

import Language.Haskell.TH
import Foreign.Storable.PeekPoke
import Gpu.Vulkan.Pipeline.VertexInputState.SizeAlignment.Internal
import Data.Bool

instanceSizeAlignmentListTuple :: Int -> DecsQ
instanceSizeAlignmentListTuple n = newTypes n >>= \ts -> do
	let	tpl = tupT ts
	(isInstance ''SizeAlignmentList . (: []) =<< tpl) >>= bool
		((: []) <$> instanceD
			(cxt $ (conT ''Sizable `appT`) <$> ts)
			(conT ''SizeAlignmentList `appT` tpl) [])
		(pure [])

tupT :: [TypeQ] -> TypeQ
tupT ts = foldl appT (tupleT $ length ts) ts

newTypes :: Int -> Q [TypeQ]
newTypes = ((varT <$>) <$>) . newNames

newNames :: Int -> Q [Name]
newNames n = newName `mapM` take n ((: "") <$> ['a' .. 'z'])

instanceSizeAlignmentListUntilTuple :: Int -> DecsQ
instanceSizeAlignmentListUntilTuple n =
	newName "t" >>= \t -> newTypes n >>= \ts -> do
		let	tpl = tupT ts
		(isInstance ''SizeAlignmentListUntil . (: []) =<< tpl) >>= bool
			((: []) <$> instanceD
				(cxt [conT ''MapSizableUntil `appT` varT t `appT` tpl])
--					varT t `appT` promotedListT ts])
				(conT ''SizeAlignmentListUntil `appT`
					varT t `appT` tpl) [])
			(pure [])
