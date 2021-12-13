{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

a :: IO ()
a = let ?x = 456 in c

c :: (?x :: Int) => IO ()
c = print ?x

d :: IO ()
d = do	let ?x = 123
	a
	c
	let ?x = 789
	c
