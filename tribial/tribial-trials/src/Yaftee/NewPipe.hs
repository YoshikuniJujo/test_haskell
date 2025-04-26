{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
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

yield :: Union.Member Yield effs => o -> Eff.E effs i o ()
yield = Eff.effh . Yield

await :: Union.Member Await effs => Eff.E effs i o i
await = Eff.effh Await

{-
(=$=) :: Union.Member P effs =>
	Yieldable (Eff.E effs) i x r -> Awaitable (Eff.E effs) x o r ->
	Eff.E effs i o (Yieldable (Eff.E effs) i x r, Awaitable (Eff.E effs) x o r)
	-}
(=$=) :: Eff.E (Yield ': effs) i x r -> Eff.E (Await ': effs) x o r ->
	Eff.E (P ': effs) i o (Eff.E (Yield ': effs) i x r, Eff.E (Await ': effs) x o r)
o =$= p = Eff.effh $ o :=$= p

(=@=) :: Union.Member P effs =>
	Yieldable (Eff.E effs) i x r -> Awaitable (Eff.E effs) x o r ->
	Eff.E effs i o (Yieldable (Eff.E effs) i x r, Awaitable (Eff.E effs) x o r)
o =@= p = Eff.effh $ o :=@= p
