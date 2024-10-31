{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Khr.Surface.Internal (
	S(..), group, unsafeDestroy, lookup, Group(..),

	M.Capabilities(..),
	Format(..), formatListToNew, formatFilter ) where

import Prelude hiding (lookup)
import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.TypeLevel.Tuple.Uncurry
import Data.Proxy
import Data.Maybe
import Data.HeteroParList.Constrained (pattern (:^*))
import Data.HeteroParList.Constrained qualified as HeteroParListC
import Data.Map qualified as Map

import Gpu.Vulkan.TypeEnum qualified as T
import Gpu.Vulkan.Instance.Internal qualified as Instance
import Gpu.Vulkan.AllocationCallbacks.Internal qualified as AllocationCallbacks
import Gpu.Vulkan.Khr.Surface.Enum
import Gpu.Vulkan.Khr.Surface.Type
import Gpu.Vulkan.Khr.Surface.Middle qualified as M

data Group si ma s k = Group (Instance.I si)
	(TPMaybe.M (U2 AllocationCallbacks.A) ma)
	TSem (TVar (Map.Map k (S s)))

group :: AllocationCallbacks.ToMiddle ma =>
	Instance.I si -> TPMaybe.M (U2 AllocationCallbacks.A) ma ->
	(forall s . Group si ma s k -> IO a) -> IO a
group i@(Instance.I mi) mac@(AllocationCallbacks.toMiddle -> ma) f = do
	(sem, m) <- atomically $ (,) <$> newTSem 1 <*> newTVar Map.empty
	rtn <- f $ Group i mac sem m
	((\(S s) -> M.destroy mi s ma) `mapM_`) =<< atomically (readTVar m)
	pure rtn

unsafeDestroy :: (Ord k, AllocationCallbacks.ToMiddle ma) =>
	Group si ma s k -> k -> IO (Either String ())
unsafeDestroy (Group (Instance.I mi) (AllocationCallbacks.toMiddle -> ma) sem ss) k =
	do	mbs <- atomically do
			mx <- Map.lookup k <$> readTVar ss
			case mx of
				Nothing -> pure Nothing
				Just _ -> waitTSem sem >> pure mx
		case mbs of
			Nothing -> pure $ Left
				"Gpu.Vulkan.Khr.Surface.Internal.unsafeDestroy: No such key"
			Just (S ms) -> do
				M.destroy mi ms ma
				atomically do
					modifyTVar ss (Map.delete k)
					signalTSem sem
					pure $ Right ()

lookup :: Ord k => Group si ma s k -> k -> IO (Maybe (S s))
lookup (Group _ _ _sem ss) k = atomically $ Map.lookup k <$> readTVar ss

data Format (fmt :: T.Format) =
	Format { formatColorSpace :: ColorSpace }

instance T.FormatToValue fmt => Show (Format fmt) where
	show (Format cs) =
		"(Format {- " ++ show (T.formatToValue @fmt) ++ " -} " ++
		show cs ++ ")"

formatToNew :: M.Format ->
	(forall fmt . T.FormatToValue fmt => Format fmt -> a) -> a
formatToNew (M.Format fmt cs) f = T.formatToType fmt \(_ :: Proxy fmt) -> f $ Format @fmt cs

formatListToNew :: [M.Format] -> (forall fmts .
	Show (HeteroParListC.PL T.FormatToValue Format fmts) =>
	HeteroParListC.PL T.FormatToValue Format fmts -> a) -> a
formatListToNew [] f = f HeteroParListC.Nil
formatListToNew (fmt : fmts) f = formatToNew fmt \fmt' ->
	formatListToNew fmts \fmts' -> f $ fmt' :^* fmts'

formatMatched :: forall fmt . T.FormatToValue fmt =>
	M.Format -> Maybe (Format fmt)
formatMatched (M.Format fmt cs)
	| T.formatToValue @fmt == fmt = Just $ Format cs
	| otherwise = Nothing

formatFilter :: forall fmt . T.FormatToValue fmt =>
	[M.Format] -> [Format fmt]
formatFilter = catMaybes . (formatMatched <$>)

{-
type FormatConstraint fmt = (
	T.FormatToValue fmt,
	MaybeFormat fmt )

-- class FilterFormat (fmts :: [T.Format]) where
--	filterFormat :: HeteroParListC.

class MaybeFormat (fmt0 :: T.Format) (fmt :: T.Format) where
	maybeFormat :: FormatNew fmt -> Maybe (FormatNew fmt0)

instance MaybeFormat fmt fmt where maybeFormat = Just

instance {-# OVERLAPPABLE #-} MaybeFormat fmt0 fmt where maybeFormat _ = Nothing
-}
