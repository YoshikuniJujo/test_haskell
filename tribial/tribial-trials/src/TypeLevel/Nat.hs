{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TypeLevel.Nat where

import GHC.TypeNats
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
