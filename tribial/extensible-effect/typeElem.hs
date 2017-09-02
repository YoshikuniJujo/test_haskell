{-# LANGUAGE KindSignatures, TypeOperators, DataKinds, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

infixr 1 :>
data ((a :: * -> *) :> b)

class (Elem t r ~ 'True) => Member (t :: * -> *) r
instance (Elem t r ~ 'True) => Member t r

type family Elem (t :: * -> *) r :: Bool where
	Elem t (t :> r) = 'True
	Elem t () = 'False
	Elem t (t' :> r) = Elem t r

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
