{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.PNext.Middle.Internal (

	-- * STRUCT COMMON

	StructCommon(..), structCommonFromCore,

	-- * SET AND READ CHAIN

	Typeable(..), FindChainAll(..), ReadChain(..), Nextable(..)

	) where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Data.Kind
import Data.HeteroParList (pattern (:**))
import Data.HeteroParList qualified as HPList

import Gpu.Vulkan.Enum
import Gpu.Vulkan.Core qualified as C

import Data.TypeLevel.Maybe qualified as TMaybe

-- STRUCT COMMON

data StructCommon = StructCommon {
	structCommonSType :: StructureType, structCommonPNext :: Ptr () }
	deriving Show

instance Peek StructCommon where
	peek' p = structCommonFromCore <$> peek (castPtr p)

instance Poke StructCommon where
	poke' p = poke (castPtr p) . structCommonToCore

structCommonFromCore :: C.StructCommon -> StructCommon
structCommonFromCore C.StructCommon {
	C.structCommonSType = stp, C.structCommonPNext = pn } = StructCommon {
	structCommonSType = StructureType stp, structCommonPNext = pn }

structCommonToCore :: StructCommon -> C.StructCommon
structCommonToCore StructCommon {
	structCommonSType = StructureType stp,
	structCommonPNext = pn } = C.StructCommon {
	C.structCommonSType = stp, C.structCommonPNext = pn }

-- TYPEABLE

class Peek n => Typeable n where structureType :: StructureType

-- FIND PNEXT CHAIN ALL

class FindChainAll ns where findChainAll :: Ptr () -> IO (HPList.PL Maybe ns)

instance FindChainAll '[] where findChainAll _ = pure HPList.Nil

instance (Typeable n, FindChainAll ns) => FindChainAll (n ': ns) where
	findChainAll p = (:**) <$> findChain p <*> findChainAll p

findChain :: forall n . Typeable n => Ptr () -> IO (Maybe n)
findChain NullPtr = pure Nothing
findChain p = peek' (castPtr p) >>= \sc ->
	if structCommonSType sc == structureType @n
		then Just <$> peek' (castPtr p)
		else findChain $ structCommonPNext sc

-- READ CHAIN

class ReadChain mn where
	clearedChain :: (Ptr () -> IO a) -> IO a
	readChain :: Ptr () -> IO (TMaybe.M mn)

instance ReadChain 'Nothing where
	clearedChain = ($ nullPtr); readChain _ = pure TMaybe.N

instance (Nextable n, ReadChain mnn) => ReadChain ('Just (n (mnn))) where
	clearedChain f = clearedChain @mnn \np -> do
		p <- callocBytes $ nextableSize @n
		poke' p StructCommon {
			structCommonSType = nextableType @n,
			structCommonPNext = np }
		f (castPtr p) <* free p
	readChain p =
		TMaybe.J <$> (createNextable p =<< readChain =<< nextPtr @n p)
	
class Nextable (n :: Maybe Type -> Type) where
	nextableSize :: Int
	nextableType :: StructureType; nextPtr :: Ptr () -> IO (Ptr ())
	createNextable :: Ptr () -> TMaybe.M mn' -> IO (n mn')
