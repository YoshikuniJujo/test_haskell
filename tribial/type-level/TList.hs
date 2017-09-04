{-# LANGUAGE KindSignatures, PolyKinds, TypeOperators, DataKinds #-}
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TList where

import Data.Void

infixr 1 :>
data ((a :: k) :> b)

class (Elem t r ~ 'True) => Member (t :: k) r
instance (Elem t r ~ 'True) => Member t r

type family Elem (t :: k) (r :: k) :: Bool where
	Elem t (t :> r) = 'True
	Elem t Void = 'False
	Elem t (t' :> r) = Elem t r

newtype Id r x = Id x deriving Show

testMember :: Member Int r => Id r x -> Id r x
testMember x = x

sampleIdHasInt :: Id (Double :> Int :> Void) Char
sampleIdHasInt = Id 'c'

sampleIdNoInt :: Id (Double :> Bool :> Void) Char
sampleIdNoInt = Id 'c'

type family EQU (a :: k) (b :: k) :: Bool where
	EQU a a = 'True
	EQU a b = 'False

type family OnlyOne (t :: k) (r :: k) :: Bool where
	OnlyOne t (t :> r) = Not (Elem t r)
	OnlyOne t Void = 'False
	OnlyOne t (t' :> r) = OnlyOne t r

type family Not (b :: Bool) where
	Not 'False = 'True
	Not 'True = 'False

type family Set (t :: k) (x :: *) :: Bool

type instance Set Tag1 Int = 'True

type family ElemTag (t :: k) (r :: *) :: Bool where
	ElemTag t (t' :> r) = 'True
	ElemTag t Void = 'False

class Member t r => MemberU2 (tag :: k -> *) (t :: k) r | tag r -> t
instance (MemberU' (EQU t1 t2) tag t1 (t2 :> r)) => MemberU2 tag t1 (t2 :> r)

class Member t r =>
	MemberU' (f :: Bool) (tag :: k -> *) (t :: k) r | tag r -> t
instance MemberU' 'True tag (tag e) (tag e :> r)
instance (Elem t (t' :> r) ~ 'True, MemberU2 tag t r) =>
	MemberU' 'False tag t (t' :> r)

data Tag1 :: * -> *

-- testMemberU2 :: MemberU2 Tag1 Int r => Id r Char -> Id r Char
-- testMemberU2 :: MemberU2 Int r => Id r Char -> Id r Char
-- testMemberU2 :: MemberU2 Int r => Id r Char -> Id r Char
-- testMemberU2 x = x

testMemberU' :: MemberU' 'True [] String r => Id r Char -> Id r Char
testMemberU' x = x

testMemberU2 :: MemberU2 Tag1 (Tag1 m) r => Id r Char -> Id r Char
testMemberU2 x = x

-- class (Member t r) => SetMember set (t :: * -> *) r | r set -> t
-- instance (MemberU set t r) => SetMember set t r
--

testOnlyOne :: OnlyOne Int r ~ 'True => Id r Char -> Id r Char
testOnlyOne x = x

testElemTag :: ElemTag Tag1 r ~ 'True => Id r Char -> Id r Char
testElemTag x = x
