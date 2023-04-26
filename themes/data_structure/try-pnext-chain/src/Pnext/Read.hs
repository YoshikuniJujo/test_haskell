{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Pnext.Read where

import Foreign.Ptr
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Control.Monad
import Data.Int
import Data.HeteroParList qualified as HeteroParList
import Data.HeteroParList (pattern (:**))

import Pnext.Core qualified as C
import Pnext.Enum

class Peek n => Nextable n where
	nextableType :: StructureType

class FindPNextChainAll ns where
	findPNextChainAll :: Ptr () -> IO (HeteroParList.PL Maybe ns)

instance FindPNextChainAll '[] where
	findPNextChainAll _ = pure HeteroParList.Nil

instance (Nextable n, FindPNextChainAll ns) =>
	FindPNextChainAll (n ': ns) where
	findPNextChainAll p =
		(:**) <$> findPNextChain p <*> findPNextChainAll p


findPNextChain :: forall n . Nextable n => Ptr () -> IO (Maybe n)
findPNextChain p | p == nullPtr = pure Nothing
findPNextChain p = do
	sc <- peek $ castPtr p
	if C.structCommonSType sc == nextableType @n
	then Just <$> peek' (castPtr p)
	else findPNextChain $ C.structCommonPNext sc

-- INT VALUE

data IntValue ns = IntValue {
	intValueNexts :: HeteroParList.PL Maybe ns,
	intValueIntNum :: Int32 }

deriving instance Show (HeteroParList.PL Maybe ns) => Show (IntValue ns)

intValueFromCore :: FindPNextChainAll ns => C.IntValue -> IO (IntValue ns)
intValueFromCore C.IntValue {
	C.intValuePNext = p,
	C.intValueIntNum = inm } = do
	rs <- findPNextChainAll p
	pure IntValue { intValueNexts = rs, intValueIntNum = inm }

instance FindPNextChainAll ns => Peek (IntValue ns) where
	peek' = intValueFromCore <=< peek . castPtr

instance FindPNextChainAll ns => Nextable (IntValue ns) where
	nextableType = StructureTypeInt

-- FLOAT VALUE

data FloatValue ns = FloatValue {
	floatValueNexts :: HeteroParList.PL Maybe ns,
	floatValueFloatNum :: Float }

deriving instance Show (HeteroParList.PL Maybe ns) => Show (FloatValue ns)

floatValueFromCore :: FindPNextChainAll ns => C.FloatValue -> IO (FloatValue ns)
floatValueFromCore C.FloatValue {
	C.floatValuePNext = p,
	C.floatValueFloatNum = fnm } = do
	rs <- findPNextChainAll p
	pure FloatValue { floatValueNexts = rs, floatValueFloatNum = fnm }

instance FindPNextChainAll ns => Peek (FloatValue ns) where
	peek' = floatValueFromCore <=< peek . castPtr

instance FindPNextChainAll ns => Nextable (FloatValue ns) where
	nextableType = StructureTypeFloat
