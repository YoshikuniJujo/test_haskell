{-# LANGUAGE TypeOperators, DataKinds, KindSignatures, TypeFamilies, GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Eff ((:>), Emp) where

infixr 1 :>

type Emp = 'Empty

type (a :: * -> *) :> b = a '::> b

data Effs where
	(::>) :: (k -> k) -> Effs -> Effs
	Empty :: Effs

data Id (r :: Effs) a = Id a deriving Show
