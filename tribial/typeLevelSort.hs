{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Type.Set.Internal (
	-- * Type
	Set(Nil, (:~)), Numbered, numbered,
	-- * Operator
	Singleton, Insert, Merge, Map, (:-), (:+:), (:$:), Sort ) where

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

type Sort ts = FromList (MergeSort (WrapList ts))

-- type family Sort (ts :: [Type]) :: Set Type where
--	Sort '[] = 'Nil

type family FromList (ts :: [Type]) :: Set Type where
	FromList '[] = 'Nil
	FromList (x ': xs) = x ':~ FromList xs

type family MergeSort (tss :: [[Type]]) :: [Type] where
	MergeSort '[] = '[]
	MergeSort '[xs] = xs
	MergeSort xss = MergeSort (MergePairs xss)

type family MergePairs (tss :: [[Type]]) :: [[Type]] where
	MergePairs '[] = '[]
	MergePairs '[xs] = '[xs]
	MergePairs (xs ': ys ': xss) = MergeList xs ys ': MergePairs xss

type family MergeList (ts :: [Type]) (ts' :: [Type]) :: [Type] where
	MergeList '[] ys = ys
	MergeList xs '[] = xs
	MergeList (x ': xs) (x ': ys) = x ': MergeList xs ys
	MergeList (x ': xs) (y ': ys) = If (Number x <=? Number y)
		(x ': MergeList xs (y ': ys)) (y ': MergeList (x ': xs) ys)

type family WrapList (ts :: [Type]) :: [[Type]] where
	WrapList '[] = '[]
	WrapList (x ': xs) = '[x] ': WrapList xs

instance Numbered () where type Number () = 0
instance Numbered Bool where type Number Bool = 1
instance Numbered Int where type Number Int = 2
instance Numbered Double where type Number Double = 3
instance Numbered Integer where type Number Integer = 4
