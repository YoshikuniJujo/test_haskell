{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Type.Set.Internal (
	-- * Type
	Set(..), Numbered, numbered,
	-- * Operator
	Singleton, Insert, Merge, Map, (:-), (:+:), (:$:) ) where

import GHC.TypeLits (Nat, type (<=?))
import Language.Haskell.TH (
	TypeQ, DecsQ, runIO,
	conT, appT, litT, numTyLit, instanceD, cxt, tySynInstD, tySynEqn )
import Data.Kind (Type)
import Data.Type.Bool (If)
import System.Random (randomRIO)

---------------------------------------------------------------------------

-- * NUMBERED
-- * TYPE SET

---------------------------------------------------------------------------
-- NUMBERED
---------------------------------------------------------------------------

class Numbered a where type Number (a :: Type) = (r :: Nat) | r -> a

numbered :: Int -> TypeQ -> DecsQ
numbered s t =
	((: []) <$>) . instanceD (cxt []) (conT ''Numbered `appT` t) . (: [])
		$ tySynInstD . tySynEqn Nothing (conT ''Number `appT` t)
			. litT . numTyLit =<< runIO (randomRIO (0, 2 ^ s - 1))

---------------------------------------------------------------------------
-- TYPE SET
---------------------------------------------------------------------------

infixr 5 :~
data Set a = Nil | a :~ Set a

type Singleton t = t ':~ 'Nil

infixr 5 :-
type t :- ts = t `Insert` ts
type family Insert (t :: Type) (ts :: Set Type) :: Set Type where
	Insert t 'Nil = t ':~ 'Nil
	Insert t (t ':~ ts) = t ':~ ts
	Insert t (t' ':~ ts) = If
		(Number t <=? Number t') (t ':~ t' ':~ ts) (t' ':~ Insert t ts)

infixr 5 :+:
type ts :+: ts' = ts `Merge` ts'
type family Merge (ts :: Set Type) (ts' :: Set Type) :: Set Type where
	Merge ts 'Nil = ts
	Merge 'Nil ts' = ts'
	Merge (t ':~ ts) (t ':~ ts') = t ':~ Merge ts ts'
	Merge (t ':~ ts) (t' ':~ ts') = If (Number t <=? Number t')
		(t ':~ Merge ts (t' ':~ ts')) (t' ':~ Merge (t ':~ ts) ts')

infixl 4 :$:
type f :$: ts = f `Map` ts
type family Map (f :: Type -> Type) (ts :: Set Type) :: Set Type where
	Map _f 'Nil = 'Nil
	Map f (t ':~ ts) = f t ':~ f `Map` ts
