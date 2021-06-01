{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Triple where

data Void

type Not a = a -> Void

foo :: Not (Not (Not a)) -> Not a
-- foo :: (((a -> Void) -> Void) -> Void) -> a -> Void
foo f x = f \g -> g x
