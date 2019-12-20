{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Prelude hiding (abs)

import DiffList

main :: IO ()
main = do
	putStrLn $ {-# SCC "LEFT" #-} abs $ ((rep "hello" ++. rep "beautiful") ++. rep "world") ++. rep "!"
	putStrLn $ {-# SCC "RIGHT" #-} abs $ rep "hello" ++. (rep "beautiful" ++. (rep "world" ++. rep "!"))
	putStrLn $ {-# SCC "OBSERVE_LEFT" #-} abs $ ((rep "hello" ++.. rep "beautiful") ++.. rep "world") ++.. rep "!"
	putStrLn $ {-# SCC "OBSERVE_RIGHT" #-} abs $ rep "hello" ++.. (rep "beautiful" ++.. (rep "world" ++.. rep "!"))

{-

LEFT:		24
RIGHT:		24

OBSERVE_LEFT:	18 + 7 + 1 + 1 = 27
OBSERVE_RIGHT:	3 + 22 + 1 + 1 = 27

-}
