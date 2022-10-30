{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TypeNat where

import GHC.TypeLits
import Data.Proxy

withSomeNat :: SomeNat -> (forall n . KnownNat n => Proxy n -> a) -> a
withSomeNat (SomeNat p) f = f p

tryNat :: Integer -> IO ()
tryNat n = case someNatVal n of
	Just sn -> withSomeNat sn \p -> print $ natVal p
	Nothing -> error "no Nat"
