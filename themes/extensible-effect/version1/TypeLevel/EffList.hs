{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures, DataKinds, TypeOperators #-}

module TypeLevel.EffList (Effs, Base, (:>)) where

infixr 1 :>

type Base = 'Empty

type (e :: * -> *) :> es = e '::> es

data Effs = Empty | forall k . (k -> k) ::> Effs
