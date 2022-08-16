{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryValueToType where

data Proxy (t :: k) = Proxy

class TypeBool (b :: Bool) where bool :: Bool
instance TypeBool 'False where bool = False
instance TypeBool 'True where bool = True

typeBool :: Bool -> (forall b . TypeBool b => Proxy (b :: Bool) -> a) -> a
typeBool False f = f $ Proxy @_ @('False)
typeBool True f = f $ Proxy @_ @('True)

boolToBool :: Bool -> Bool
boolToBool b = typeBool b \(_ :: Proxy tb) -> bool @tb

tBoolToTBool :: TypeBool b => Proxy (b :: Bool) ->
	(forall tb . TypeBool tb => Proxy (tb :: Bool) -> a) -> a
tBoolToTBool (_ :: Proxy tb) = typeBool (bool @tb)
