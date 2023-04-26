{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Pnext.Write where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Storable.PeekPoke.Internal
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.Int

import Pnext.Core qualified as C

data IntValue mn = IntValue {
	intValueNext :: TMaybe.M mn,
	intValueIntNum :: Int32 }

deriving instance Show (TMaybe.M mn) => Show (IntValue mn)

intValueToCore :: WithPoked (TMaybe.M mn) =>
	IntValue mn -> (C.IntValue -> IO a) -> IO a
intValueToCore IntValue {
	intValueNext = mnxt, intValueIntNum = inm } f =
	withPoked' mnxt \(PtrS_ (castPtr -> pnxt)) ->
	let	iv = C.IntValue {
			C.intValueSType = (),
			C.intValuePNext = pnxt,
			C.intValueIntNum = inm } in f iv

instance WithPoked (TMaybe.M mn) => WithPoked (IntValue mn) where
	withPoked' x f = alloca \piv -> intValueToCore x $ \civ -> do
		poke piv civ
		f . ptrS $ castPtr piv

data FloatValue mn = FloatValue {
	floatValueNext :: TMaybe.M mn,
	floatValueFloatNum :: Float }

deriving instance Show (TMaybe.M mn) => Show (FloatValue mn)

floatValueToCore :: WithPoked (TMaybe.M mn) =>
	FloatValue mn -> (C.FloatValue -> IO a) -> IO a
floatValueToCore FloatValue {
	floatValueNext = mnxt, floatValueFloatNum = fnm } f =
	withPoked' mnxt \(PtrS_ (castPtr -> pnxt)) ->
	let	fv = C.FloatValue {
			C.floatValueSType = (),
			C.floatValuePNext = pnxt,
			C.floatValueFloatNum = fnm } in
	f fv

instance WithPoked (TMaybe.M mn) => WithPoked (FloatValue mn) where
	withPoked' x f = alloca \pfv -> floatValueToCore x $ \cfv -> do
		poke pfv cfv
		f . ptrS $ castPtr pfv
