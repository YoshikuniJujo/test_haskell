{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Pnext.Read where

import Foreign.Ptr
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Data.Int
import Data.HeteroParList qualified as HeteroParList

import Pnext.Core qualified as C
import Pnext.Enum

data IntValue ns = IntValue {
	intValueNexts :: HeteroParList.PL Maybe ns,
	intValueIntNum :: Int32 }

deriving instance Show (HeteroParList.PL Maybe ns) => Show (IntValue ns)

-- intValueFromCore :: C.IntValue -> IO (IntValue ns)

-- instance Nextable (IntValue ns) where
--	nextableType = StructureTypeInt

class Peek n => Nextable n where
	nextableType :: StructureType

findPNextChain :: forall n . Nextable n => Ptr () -> IO (Maybe n)
findPNextChain p | p == nullPtr = pure Nothing
findPNextChain p = do
	sc <- peek $ castPtr p
	if C.structCommonSType sc == nextableType @n
	then Just <$> peek' (castPtr p)
	else findPNextChain $ C.structCommonPNext sc
