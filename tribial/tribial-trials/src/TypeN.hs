{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}


module TypeN where

import Data.Proxy
import Numeric.Natural

data N = Z | S N deriving Show

class ShowN (n :: N) where showN :: Natural
instance ShowN Z where showN = 0
instance ShowN n => ShowN (S n) where showN = showN @n + 1

readN :: Natural -> (forall n . Proxy (n :: N) -> a) -> a
readN 0 f = f (Proxy :: Proxy Z)
readN n f = readN (n - 1) \(_ :: Proxy n) -> f (Proxy :: Proxy (S n))

readN2 :: Natural -> Natural ->
	(forall n m . (Add n Z, Add n m) => Proxy (n :: N) -> Proxy (m :: N) -> a) -> a
readN2 0 0 f = f (Proxy :: Proxy Z) (Proxy :: Proxy Z)
readN2 n 0 f =
	readN2 (n - 1) 0 \(_ :: Proxy n') (_ :: Proxy m) -> f (Proxy :: Proxy (S n')) (Proxy :: Proxy Z)
readN2 n m f =
	readN2 n (m - 1) \(_ :: Proxy n) (_ :: Proxy m') -> f (Proxy :: Proxy n) (Proxy :: Proxy (S m'))

class Add (n :: N) (m :: N) where
	add :: Proxy n -> Proxy m -> (forall nm . ShowN nm => Proxy nm -> a) -> a

instance Add Z Z where add _ _ f = f (Proxy :: Proxy Z)

instance Add n Z => Add (S n) Z where
	add _ _ f = add (Proxy :: Proxy n) (Proxy :: Proxy Z) \(_ :: Proxy nm) -> f (Proxy :: Proxy (S nm))

instance Add n m => Add n (S m) where
	add _ _ f = add (Proxy :: Proxy n) (Proxy :: Proxy m) \(_ :: Proxy nm) -> f (Proxy :: Proxy (S nm))

{-
type family Add n m where
	Add Z Z = Z
	Add (S n) Z = S (Add n Z)
	Add n (S m) = S (Add n m)
	-}

getN :: (forall n . Proxy (n :: N) -> IO a) -> IO a
getN f = do
	n <- read <$> getLine
	readN n f

getN2 :: (forall n m . (Add n Z, Add n m) => Proxy (n :: N) ->
	Proxy (m :: N) -> IO a) -> IO a
getN2 f = do
	n <- read <$> getLine
	m <- read <$> getLine
	readN2 n m f

youCanAdd :: IO ()
youCanAdd =
--	getN \(_ :: Proxy x) ->
--	getN \(_ :: Proxy y) ->
	getN2 \(_ :: Proxy x) (_ :: Proxy y) ->
	add (Proxy :: Proxy x) (Proxy :: Proxy y) \(_ :: Proxy xy) -> print $ showN @xy
