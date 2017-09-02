{-# LANGUAGE KindSignatures, TypeOperators, DataKinds, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE PolyKinds, FunctionalDependencies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

{-# LANGUAGE UndecidableInstances #-}

module TypeElem ((:>), Member, MemberU2) where

infixr 1 :>
data ((a :: * -> *) :> b)

class (Elem t r ~ 'True) => Member (t :: * -> *) r
instance (Elem t r ~ 'True) => Member t r

type family Elem (t :: * -> *) r :: Bool where
	Elem t (t :> r) = 'True
	Elem t () = 'False
	Elem t (t' :> r) = Elem t r

class Member t r => MemberU2 (tag :: k -> * -> *) (t :: * -> *) r | tag r -> t
instance (MemberU' (EQU t1 t2) tag t1 (t2 :> r)) => MemberU2 tag t1 (t2 :> r)

class Member t r =>
	MemberU' (f :: Bool) (tag :: k -> * -> *) (t :: * -> *) r | tag r -> t
instance MemberU' True tag (tag e) (tag e :> r)
instance (Elem t (t' :> r) ~ True, MemberU2 tag t r) =>
	MemberU' False tag t (t' :> r)

type family EQU (a :: k) (b :: k) :: Bool where
	EQU a a = True
	EQU a b = False

{-
newtype Foo r a = Foo a deriving Show

data You a

iNeedYou :: Elem You r ~ 'True => Foo r a -> Foo r a
iNeedYou = id

iNeedYou' :: Member You r => Foo r a -> Foo r a
iNeedYou' = id

iNeedYou'' :: Member You r => Foo r a -> Foo r a
iNeedYou'' = iNeedYou . iNeedYou'

iNeedYou''' :: Elem You r ~ 'True => Foo r a -> Foo r a
iNeedYou''' = iNeedYou' . iNeedYou
-}
