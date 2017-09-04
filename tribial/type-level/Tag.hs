{-# LANGUAGE TypeFamilies, DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tag where

data Number

type family Tag t a :: Bool where
	Tag Number Int = 'True
	Tag t a = 'False

testTag :: Tag Number n ~ 'True => n -> n
testTag x = x

type family Elem t lst :: Bool where
	Elem t (t : lst') = 'True
	Elem t (t' : lst') = Elem t lst'
	Elem t '[] = 'False

newtype Id (r :: [*]) a = Id a deriving Show

testElem :: Elem Int lst ~ 'True => Id lst Bool -> Id lst Bool
testElem x = x

type family ElemTagGen (b :: Bool) t lst :: Bool where
	ElemTagGen 'True t lst = 'True
	ElemTagGen 'False t (t' : lst) = ElemTagGen (Tag t t') t lst
	ElemTagGen b t '[] = b

type ElemTag t lst = ElemTagGen 'False t lst

testElemTag :: ElemTag Number lst ~ 'True => Id lst Bool -> Id lst Bool
testElemTag x = x
