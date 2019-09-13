{-# LANGUAGE GADTs, ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data ROr a b where
	L :: a => ROr a b
	R :: b => ROr a b

type Or a b = (?choose :: ROr a b)

y :: Or (a ~ Integer) (Bool ~ Integer) => a
y = case ?choose of L -> 4

x :: Integer
x = let ?choose = L in y
