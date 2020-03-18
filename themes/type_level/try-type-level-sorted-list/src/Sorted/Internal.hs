-- {-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds, TypeFamilies, TypeFamilyDependencies, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Sorted.Internal (Sorted(..), Singleton, Insert, Merge, Map, Numbered, numbered) where

import GHC.TypeLits
import Language.Haskell.TH hiding (Type)
import Data.Kind
import Data.Type.Bool
import System.Random

-- type Number :: Type -> Nat
-- type family Number a = n | n -> a

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

type family Merge (ts :: Sorted Type) (ts' :: Sorted Type) :: Sorted Type where
	Merge ts 'Nil = ts
	Merge 'Nil ts' = ts'
	Merge (t ':~ ts) (t ':~ ts') = t ':~ Merge ts ts'
	Merge (t ':~ ts) (t' ':~ ts') = If (Number t <=? Number t')
		(t ':~ Merge ts (t' ':~ ts'))
		(t' ':~ Merge (t ':~ ts) ts')
--		(t ':~ t' ':~ Merge ts ts')
--		(t' ':~ t ':~ Merge ts ts')

type family Map (f :: Type -> Type) (ts :: Sorted Type) :: Sorted Type where
	Map _f 'Nil = 'Nil
	Map f (t ':~ ts) = f t ':~ Map f ts

instance Numbered () where
	type instance Number () = 108

instance Numbered Int where
	type instance Number Int = 115

instance Numbered Double where
	type instance Number Double = 107

instance Numbered Bool where
	type instance Number Bool = 111

instance Numbered Integer where
	type instance Number Integer = 109
