{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies, DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

{-# OPTIONS_GHC-Wall -fno-warn-tabs #-}

module HeteroList (empty, (.:), get) where

infixr 5 :.:

data (:.:) a b = a :.: b deriving Show

type family Elem t ts where
	Elem _ () = 'False
	Elem t (t :.: _) = 'True
	Elem t (_ :.: ts) = Elem t ts

empty :: ()
empty = ()

infixr 5 .:

(.:) :: Elem t ts ~ 'False => t -> ts -> (t :.: ts)
(.:) = (:.:)

class Get v vs where get :: vs -> v

instance Get v (v :.: vs) where
	get (x :.: _) = x
instance {-# OVERLAPPABLE #-} Get v vs => Get v (_w :.: vs) where
	get (_ :.: xs) = get xs

_sample :: Int :.: Double :.: Bool :.: Char :.: ()
_sample = 3 .: 8.5 .: False .: 'c' .: ()
