{-# LANGUAGE TypeFamilies, TypeFamilyDependencies, KindSignatures, DataKinds, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import GHC.TypeLits
import Data.Kind
import Data.Type.Bool
import Data.Proxy

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type family Number a :: Nat
-- type family Number a = n | n -> a

type instance Number () = 8
type instance Number Int = 15
type instance Number Double = 4

data Tree a = Tip | Node a (Tree a) (Tree a) deriving Show

type family Insert (t :: Type) (tr :: Tree Type) :: Tree Type where
	Insert t 'Tip = 'Node t 'Tip 'Tip
	Insert t ('Node t' l r) = If (Number t <=? Number t')
		(If (Number t' <=? Number t)
			('Node t' l r)
			('Node t' (Insert t l) r))
		('Node t' l (Insert t r))

sample0 :: Proxy (Insert Double (Insert Int (Insert Double (Insert () 'Tip))))
sample0 = Proxy
