{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, RankNTypes #-}
{-# LANGUAGE GADTs, TypeFamilies, ExistentialQuantification #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes, InstanceSigs #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryHeteroMap2 where

import Data.Kind

infixr 5 :.

data List (t :: k -> Type) ss where
	Nil :: List t '[]
	(:.) :: t s -> List t ss -> List t (s ': ss)

instance Show (List t '[]) where show Nil = "Nil"

instance (Show (t s), Show (List t ss)) => Show (List t (s ': ss)) where
	show (t :. ts) = show t ++ " :. " ++ show ts

data Message s = Message String deriving Show

newtype Str (d :: ()) = Str String deriving Show

makeMessage :: String -> (forall s . Message s -> IO a) -> IO a
makeMessage str f = do
	putStrLn "MAKE MESSAGE"
	f (Message str)
		<* putStrLn "FREE MESSAGE"

class MakeMessages f (a :: Type) where
	type Strings f :: [()]
	makeMessages :: List Str (Strings f) -> f -> IO a

instance MakeMessages (IO a) a where
	type Strings (IO a) = '[]
	makeMessages Nil act = act

data NextFun f = NextFun (forall s . Message s -> f)

instance MakeMessages f a => MakeMessages (NextFun f) a where
	type Strings (NextFun f) = '() ': Strings f
	makeMessages (Str str :. strs) (NextFun f) = makeMessage str \msg ->
		makeMessages @f @a strs (f msg)

addNextFun2 :: (forall a b . (Show a, Show b) => a -> b -> c) -> NextFun (NextFun c)
addNextFun2 f = NextFun \a -> NextFun \b -> f a b
