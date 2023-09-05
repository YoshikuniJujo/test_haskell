{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.PNext.Middle.Internal (

	-- * STRUCT COMMON

	StructCommon(..), structCommonFromCore,

	-- * FIND P NEXT CHAIN ALL

	FindPNextChainAll(..), Nextable(..)

	) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Data.HeteroParList (pattern (:**))
import Data.HeteroParList qualified as HeteroParList

import Gpu.Vulkan.Enum
import Gpu.Vulkan.Core qualified as C

data StructCommon = StructCommon {
	structCommonSType :: StructureType,
	structCommonPNext :: Ptr () }
	deriving Show

structCommonFromCore :: C.StructCommon -> StructCommon
structCommonFromCore C.StructCommon {
	C.structCommonSType = stp,
	C.structCommonPNext = pn } = StructCommon {
	structCommonSType = StructureType stp,
	structCommonPNext = pn }

instance Peek StructCommon where
	peek' p = structCommonFromCore <$> peek (castPtr p)

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
