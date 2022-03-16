{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Foreign.Pointable where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Control.Monad.Cont

class Pointable a where
	withPointer :: a -> (Ptr a -> IO b) -> IO b

instance {-# OVERLAPPABLE #-} Storable a => Pointable a where
	withPointer x f = alloca \p -> poke p x >> f p

maybeToPointer :: Pointable a => Maybe a -> ContT r IO (Ptr a)
maybeToPointer = \case Nothing -> pure NullPtr; Just x -> ContT $ withPointer x

pointerToMaybe :: Storable a => Ptr a -> IO (Maybe a)
pointerToMaybe = \case NullPtr -> pure Nothing; p -> Just <$> peek p

pattern NullPtr :: Ptr a
pattern NullPtr <- ((== nullPtr) -> True) where NullPtr = nullPtr

pattern NullFunPtr :: FunPtr a
pattern NullFunPtr <- ((== nullFunPtr) -> True) where NullFunPtr = nullFunPtr
