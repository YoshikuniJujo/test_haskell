{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Type.Set.Internal (
	-- * Type
	Set(Nil, (:~)), Numbered, numbered,
	-- * Operator
	Singleton, Insert, Merge, Map, (:-), (:+:), (:$:) ) where

import GHC.TypeLits (Nat, type (<=?))
import Language.Haskell.TH (
	TypeQ, DecsQ, runIO,
	instanceD, cxt, tySynInstD, tySynEqn, conT, appT, litT, numTyLit )
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
numbered s t = ((: []) <$>)
	. instanceD (cxt []) (conT ''Numbered `appT` t) . (: [])
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
--	Insert t (t' ':~ ts) =
--		BOOL (InsertElse t' (Insert t ts)) (InsertThen t t' ts) $ (Number t <=? Number t')
	Insert t (t' ':~ ts) = If
		(Number t <=? Number t') (t ':~ t' ':~ ts) (t' ':~ Insert t ts)

data InsertElse t' i :: () >-> k
type instance InsertElse  t' i $ '() = t' ':~ i

data InsertThen t t' ts :: () >-> k
type instance InsertThen t t' ts $ '() = t ':~ t' ':~ ts

infixr 5 :+:
type ts :+: ts' = ts `Merge` ts'
type family Merge (ts :: Set Type) (ts' :: Set Type) :: Set Type where
	Merge ts 'Nil = ts
	Merge 'Nil ts' = ts'
	Merge (t ':~ ts) (t ':~ ts') = t ':~ Merge ts ts'
	{-
	Merge (t ':~ ts) (t' ':~ ts') = BOOL
		(ConsMerge' t ts t' ts')
		(ConsMerge t ts t' ts')
		$ (Number t <=? Number t')
		-}
	Merge (t ':~ ts) (t' ':~ ts') = If (Number t <=? Number t')
		(t ':~ Merge ts (t' ':~ ts')) (t' ':~ Merge (t ':~ ts) ts')

infixl 4 :$:
type f :$: ts = f `Map` ts
type family Map (f :: Type -> Type) (ts :: Set Type) :: Set Type where
	Map _f 'Nil = 'Nil
	Map f (t ':~ ts) = f t ':~ f `Map` ts

---------------------------------------------------------------------------
-- BOOL
---------------------------------------------------------------------------

type a >-> b = (b -> Type) -> a -> Type
type family ($) (f :: a >-> b) (x :: a) :: b

data BOOL :: (() >-> k) -> (() >-> k) -> (Bool >-> k)
type instance (BOOL f _) $ 'False = f $ '()
type instance (BOOL _ t) $ 'True = t $ '()

data ConsMerge t ts t' ts' :: () >-> k
type instance ConsMerge t ts t' ts' $ '() = t ':~ Merge ts (t' ':~ ts')

data ConsMerge' t ts t' ts' :: () >-> k
type instance ConsMerge' t ts t' ts' $ '() = t' ':~ Merge (t :~ ts) ts'
