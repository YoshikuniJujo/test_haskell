{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE PolyKinds, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Type.Set.Internal (
	-- * Set
	Set(Nil, (:~)),
	-- * Numbered
	Numbered, numbered,
	-- * Function
	Singleton, Insert, Merge, Map,
	-- * Operator
	(:-), (:+:) ) where

import GHC.TypeLits (Nat, type (<=?))
import Language.Haskell.TH (
	TypeQ, DecsQ, runIO,
	instanceD, cxt, tySynInstD, tySynEqn, conT, appT, litT, numTyLit )
import Data.Kind (Type)
import System.Random (randomRIO)

---------------------------------------------------------------------------

-- * TYPE SET
--	+ DATA DEFINITION
--	+ COMBINATOR
--		- Singleton
--		- Insert
--		- Merge
--		- Map
-- * NUMBERED
-- * BOOL

---------------------------------------------------------------------------
-- TYPE SET
---------------------------------------------------------------------------

-- DATA DEFINITION

infixr 5 :~
data Set a = Nil | a :~ Set a

-- COMBINATOR

-- Singleton

type Singleton t = t ':~ 'Nil

-- Insert

infixr 5 :-
type t :- ts = t `Insert` ts

type family Insert (t :: Type) (ts :: Set Type) :: Set Type where
	Insert t 'Nil = t ':~ 'Nil
	Insert t (t ':~ ts) = t ':~ ts
	Insert t (t' ':~ ts) = BOOL
		(InsertElse t t' ts)
		(InsertThen t t' ts)
			$ (Number t <=? Number t')

data InsertElse t t' ts :: () >-> k
type instance InsertElse  t t' ts $ '() = t' ':~ t :- ts

data InsertThen t t' ts :: () >-> k
type instance InsertThen t t' ts $ '() = t ':~ t' ':~ ts

-- Merge

infixr 5 :+:
type ts :+: ts' = ts `Merge` ts'

type family Merge (ts :: Set Type) (ts' :: Set Type) :: Set Type where
	Merge ts 'Nil = ts
	Merge 'Nil ts' = ts'
	Merge (t ':~ ts) (t ':~ ts') = t ':~ Merge ts ts'
	Merge (t ':~ ts) (t' ':~ ts') = BOOL
		(MergeElse t ts t' ts')
		(MergeThen t ts t' ts')
			$ (Number t <=? Number t')

data MergeElse t ts t' ts' :: () >-> k
type instance MergeElse t ts t' ts' $ '() = t' ':~ (t ':~ ts) :+: ts'

data MergeThen t ts t' ts' :: () >-> k
type instance MergeThen t ts t' ts' $ '() = t ':~ ts :+: (t' ':~ ts')

-- Map

type family Map (f :: Type -> Type) (ts :: Set Type) :: Set Type where
	_ `Map` 'Nil = 'Nil
	f `Map` (t ':~ ts) = f t ':~ (f `Map` ts)

---------------------------------------------------------------------------
-- NUMBERED
---------------------------------------------------------------------------

class Numbered a where type Number (a :: Type) = (r :: Nat) | r -> a

numbered :: TypeQ -> DecsQ
numbered t = ((: []) <$>)
	. instanceD (cxt []) (conT ''Numbered `appT` t) . (: [])
		$ tySynInstD . tySynEqn Nothing (conT ''Number `appT` t)
			. litT . numTyLit =<< runIO (randomRIO (0, 2 ^ s - 1))
	where s = 64 :: Int

---------------------------------------------------------------------------
-- BOOL
---------------------------------------------------------------------------

type a >-> b = (b -> Type) -> a -> Type
type family ($) (f :: a >-> b) (x :: a) :: b

data BOOL :: (() >-> k) -> (() >-> k) -> (Bool >-> k)
type instance (BOOL f _) $ 'False = f $ '()
type instance (BOOL _ t) $ 'True = t $ '()
