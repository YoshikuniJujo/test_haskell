{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TypeLevel.Nat where

import GHC.TypeNats
import Data.Type.Bool
import Data.Proxy

data Foo (n :: Nat) = Foo String deriving Show

printNat :: KnownNat n => Foo n -> IO ()
printNat f = print $ natVal f

getNat :: (forall n . KnownNat n => Foo n -> IO a) -> IO a
getNat f = do
	n <- read <$> getLine
	($ someNatVal n) \(SomeNat (_ :: Proxy n)) -> f (Foo "Hello" :: Foo n)

getNatAndPrint :: IO ()
getNatAndPrint = getNat printNat

class MyNatVal (n :: Nat) where myNatVal :: Natural

instance MyNatVal 0 where myNatVal = 0

instance {-# OVERLAPPABLE #-} MyNatVal (n - 1) => MyNatVal n where
	myNatVal = myNatVal @(n - 1) + 1

{-
mySomeNatVal :: Natural -> (forall n . MyNatVal n => Proxy n -> a) -> a
mySomeNatVal 0 f = f $ Proxy @0
mySomeNatVal n f = mySomeNatVal (n - 1) \(_ :: Proxy n) -> f $ Proxy @(n + 1)
-}

{-
getMyNat :: (forall n . (KnownNat n, MyNatVal n) => Foo n -> IO a) -> IO a
getMyNat f = do
	n <- read <$> getLine
	($ someNatVal n) \(SomeNat (_ :: Proxy n)) -> f (Foo "Hello" :: Foo n)
	-}

-- type Max (a :: k) (b :: k) = If (a <=? b) b a
