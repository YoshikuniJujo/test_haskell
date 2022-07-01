{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryHeteroMap where

import Data.Kind

infixr 5 :.

data List t ss where
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

class MakeMessages f where
	type Strings f :: [()]
	type F f
	makeMessages :: List Str (Strings f) -> F f

instance MakeMessages (IO a) where
	type Strings (IO a) = '[]
	type F (IO a) = IO a -> IO a
--	makeMessages :: Strings (IO a) -> F (IO a)
	makeMessages Nil = id

-- instance MakeMessages f => MakeMessages (forall (s :: Type) . Message s -> f) where
