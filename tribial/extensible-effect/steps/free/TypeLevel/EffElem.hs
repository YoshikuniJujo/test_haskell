{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures, DataKinds, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TypeLevel.EffElem (Member) where

import TypeLevel.EffList

class (Elem t r ~ 'True) => Member (t :: * -> *) r
instance (Elem t r ~ 'True) => Member t r

type family Elem (t :: * -> *) (r :: Effs) :: Bool where
	Elem t (t :> r) = 'True
	Elem t Emp = 'False
	Elem t (t' :> r) = Elem t r
