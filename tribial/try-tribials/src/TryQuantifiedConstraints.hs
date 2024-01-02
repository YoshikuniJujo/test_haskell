{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryQuantifiedConstraints where

import Data.Proxy

class Check (b :: Bool) where check :: String
instance Check True where check = "True"
instance {-# OVERLAPPABLE #-} Check b where check = "False"

foo :: Bool -> (forall (b :: Bool) . Check b => Proxy b -> a) -> a
foo False f = f (Proxy :: Proxy False)
foo True f = f (Proxy :: Proxy True)

bar :: Bool -> String
bar b = foo b (\(_ :: Proxy b) -> check @b)

class Same (b0 :: Bool) (b :: Bool) where same :: Bool
instance Same False False where same = True
instance Same True True where same = True
instance {-# OVERLAPPABLE #-} Same b0 b where same = False

baz :: (Same b0 'False, Same b0 'True) =>
	Bool -> (forall (b :: Bool) . Same b0 b => Proxy b -> a) -> a
baz False f = f (Proxy :: Proxy False)
baz True f = f (Proxy :: Proxy True)

boo :: forall (b0 :: Bool) . (Same b0 False, Same b0 True) => (Bool, Bool)
boo = (same @b0 @False, same @b0 @True)

bee :: forall (b0 :: Bool) . (forall b . Same b0 b) => (Bool, Bool)
bee = (same @b0 @False, same @b0 @True)
