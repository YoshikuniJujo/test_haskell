{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.PNext.Middle.Internal (

	-- * STRUCT COMMON

	StructCommon(..), structCommonFromCore,

	-- * FIND P NEXT CHAIN ALL

	FindPNextChainAll(..), Nextable(..),

	FindPNextChainAll'(..), Nextable'(..),

	-- * OTHERS

	ClearedChain(..)

	) where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Data.Kind
import Data.HeteroParList (pattern (:**))
import Data.HeteroParList qualified as HeteroParList

import Gpu.Vulkan.Enum
import Gpu.Vulkan.Core qualified as C

import Data.TypeLevel.Maybe qualified as TMaybe

data StructCommon = StructCommon {
	structCommonSType :: StructureType,
	structCommonPNext :: Ptr () }
	deriving Show

instance Peek StructCommon where
	peek' p = structCommonFromCore <$> peek (castPtr p)

structCommonFromCore :: C.StructCommon -> StructCommon
structCommonFromCore C.StructCommon {
	C.structCommonSType = stp,
	C.structCommonPNext = pn } = StructCommon {
	structCommonSType = StructureType stp,
	structCommonPNext = pn }

instance Poke StructCommon where
	poke' p = poke (castPtr p) . structCommonToCore

structCommonToCore :: StructCommon -> C.StructCommon
structCommonToCore StructCommon {
	structCommonSType = StructureType stp,
	structCommonPNext = pn } = C.StructCommon {
	C.structCommonSType = stp,
	C.structCommonPNext = pn }

class Peek n => Nextable n where nextableType :: StructureType

class FindPNextChainAll ns where
	findPNextChainAll :: Ptr () -> IO (HeteroParList.PL Maybe ns)

instance FindPNextChainAll '[] where
	findPNextChainAll _ = pure HeteroParList.Nil

instance (Nextable n, FindPNextChainAll ns) =>
	FindPNextChainAll (n ': ns) where
	findPNextChainAll p =
		(:**) <$> findPNextChain p <*> findPNextChainAll p

findPNextChain :: forall n . Nextable n => Ptr () -> IO (Maybe n)
findPNextChain NullPtr = pure Nothing
findPNextChain p = do
	sc <- peek' $ castPtr p
	putStrLn "findPNextChain"
	putStrLn $ "\tthis type    : " ++ show (structCommonSType sc)
	putStrLn $ "\tnextable type: " ++ show (nextableType @n)
	if structCommonSType sc == nextableType @n
	then Just <$> peek' (castPtr p)
	else findPNextChain $ structCommonPNext sc

class ClearedChain (ns :: [Type]) where
	clearedChain :: (Ptr () -> IO a) -> IO a

instance ClearedChain '[] where clearedChain = ($ nullPtr)

instance (Sizable n, Nextable n, ClearedChain ns) => ClearedChain (n ': ns) where
	clearedChain f = clearedChain @ns \p -> do
		let	sc = StructCommon {
				structCommonSType = nextableType @n,
				structCommonPNext = p }
		p' <- callocBytes (sizeOf' @n)
		poke' p' sc
		rslt <- f $ castPtr p'
		free p'
		pure rslt

class FindPNextChainAll' mn where
	findPNextChainAll' :: Ptr () -> IO (TMaybe.M mn)

instance FindPNextChainAll' 'Nothing where
	findPNextChainAll' _ = pure TMaybe.N

instance (Nextable' n, FindPNextChainAll' mn') =>
	FindPNextChainAll' ('Just (n (mn'))) where
	findPNextChainAll' p = do
		p' <- nextPtr @n p
		mn' <- findPNextChainAll' p'
		TMaybe.J <$> createNextable p mn'
	
class Nextable' (n :: Maybe Type -> Type) where
	nextPtr :: Ptr () -> IO (Ptr ())
	createNextable :: Ptr () -> TMaybe.M mn' -> IO (n mn')
