{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Yaftee.NewPipe where

import Data.Kind
import Yaftee.Eff qualified as Eff
import Yaftee.OpenUnion qualified as Union

data Yield (f :: Type -> Type -> Type -> Type) i o a where
	Yield :: forall f i o . o -> Yield f i o ()

type family Yieldable (f :: Type -> Type -> Type -> Type) ::
	Type -> Type -> Type -> Type

type instance Yieldable (Eff.E (P ': effs)) = Eff.E (Yield ': effs)

data Await (f :: Type -> Type -> Type -> Type) i o a where
	Await :: forall f i o . Await f i o i

type family Awaitable (f :: Type -> Type -> Type -> Type) ::
	Type -> Type -> Type -> Type

type instance Awaitable (Eff.E (P ': effs)) = Eff.E (Await ': effs)

data P (f :: Type -> Type -> Type -> Type) i o a where
	(:=$=) :: Yieldable f i x r -> Awaitable f x o r ->
		P f i o (Yieldable f i x r, Awaitable f x o r)
	(:=@=) :: Yieldable f i x r -> Awaitable f x o r ->
		P f i o (Yieldable f i x r, Awaitable f x o r)
