{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=HelloTypecheckPlugin.Plugin #-}

module HelloTypecheckPlugin.Test where

data Foo a where
	FooZero :: Foo a

eval :: Foo a -> a
eval FooZero = (0 :: Int)
