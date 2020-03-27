-- {-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds, TypeFamilies, TypeFamilyDependencies, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Sorted.Internal (
	-- * Types
	Sorted(..), Numbered, numbered,
	-- * Type Level Operators
	Singleton, Insert, Merge, Map, (:-), (:+:), (:$:) ) where

import GHC.TypeLits
import Language.Haskell.TH hiding (Type)
import Data.Kind
import Data.Type.Bool
import System.Random

numbered :: TypeQ -> DecsQ
numbered t = ((: []) <$>) . instanceD (cxt []) (conT ''Numbered `appT` t) $ (: []) do
	n <- runIO $ abs <$> randomIO
	tySynInstD $ tySynEqn Nothing (conT ''Number `appT` t) (litT $ numTyLit n)

class Numbered a where
	type Number (a :: Type) = (r :: Nat) | r -> a

infixr 5 :~
data Sorted a = Nil | a :~ Sorted a

type family Singleton (t :: Type) :: Sorted Type where
	Singleton t = t ':~ 'Nil

type family Insert (t :: Type) (ts :: Sorted Type) :: Sorted Type where
	Insert t 'Nil = t ':~ 'Nil
	Insert t (t ':~ ts) = t ':~ ts
	Insert t (t' ':~ ts) = If (Number t <=? Number t')
		(t ':~ t' ':~ ts)
		(t' ':~ Insert t ts)

infixr 5 :-
type t :- ts = t `Insert` ts

type family Merge (ts :: Sorted Type) (ts' :: Sorted Type) :: Sorted Type where
	Merge ts 'Nil = ts
	Merge 'Nil ts' = ts'
	Merge (t ':~ ts) (t ':~ ts') = t ':~ Merge ts ts'
	Merge (t ':~ ts) (t' ':~ ts') = If (Number t <=? Number t')
		(t ':~ Merge ts (t' ':~ ts'))
		(t' ':~ Merge (t ':~ ts) ts')

infixr 5 :+:
type ts :+: ts' = ts `Merge` ts'

type family Map (f :: Type -> Type) (ts :: Sorted Type) :: Sorted Type where
	Map _f 'Nil = 'Nil
	Map f (t ':~ ts) = f t ':~ Map f ts

infixl 4 :$:
type f :$: t = f `Map` t
