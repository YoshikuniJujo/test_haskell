{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ExampleForFreeCoyoneda where

import FreeCoyoneda

data MyMaybe a = MyNothing | MyJust a deriving Show

checkPositive :: Integer -> MyMaybe Integer
checkPositive n
	| n > 0 = MyJust n
	| otherwise = MyNothing

sample :: Integer -> Free (Coyoneda MyMaybe) Integer
sample n = do
	m <- toFC . checkPositive $ n - 9
	return $ m + 5

runMyMaybe :: Free (Coyoneda MyMaybe) a -> MyMaybe a
runMyMaybe = \case
	Pure x -> MyJust x
	Join (Coyoneda MyNothing _) -> MyNothing
	Join (Coyoneda (MyJust x) k) -> runMyMaybe $ k x
