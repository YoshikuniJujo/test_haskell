{-# LANGUAGE DataKinds, KindSignatures, TypeOperators, PolyKinds #-}
{-# LANGUAGE TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Check.Sorted where

import GHC.TypeLits
import Data.Kind
import Data.Type.Bool

infixr 5 :~
data Sorted (num :: Type -> Nat) a = Nil | a :~ Sorted num a

type family Insert num (t :: Type) (ts :: Sorted num Type) :: Sorted num Type where
	Insert num t 'Nil = t ':~ 'Nil
	Insert num t (t ':~ ts) = t ':~ ts
	Insert num t (t' ':~ ts) = If (num t <=? num t')
		(t ':~ t' ':~ ts)
		(t' ':~ Insert num t ts)

type family Number :: Type -> Nat {- where
	Number () = 8
	Number Int = 15
	Number Double = 7
	Number Bool = 11
	Number Integer = 9
	-}

-- type instance Number Int = 8
