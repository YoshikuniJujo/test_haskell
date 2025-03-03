{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

main :: IO ()
main = do
	putStrLn "Hello, from main!"
	c_routine

haskell_definition :: IO ()
haskell_definition = putStrLn "Hello, from haskell_definition"

foreign import ccall safe "prototypes.h"
	c_routine :: IO ()

foreign export ccall haskell_definition :: IO ()
