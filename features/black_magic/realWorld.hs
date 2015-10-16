{-# LANGUAGE MagicHash, UnboxedTuples #-}

import GHC.Prim
import GHC.Types

some :: ()
some = let
	IO m = putStrLn "hello"
	(# _, r #) = m realWorld# in
	r

main :: IO ()
main = return $! some
