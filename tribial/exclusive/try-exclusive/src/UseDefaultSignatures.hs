{-# LANGUAGE DefaultSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module UseDefaultSignatures where

class SomeClass a where
	hello :: a -> IO ()

	default hello :: Foo a => a -> IO ()
	hello = foo

	default hello :: Bar a => a -> IO ()
	hello = bar

class Foo a where
	foo :: a -> IO ()

class Bar a where
	bar :: a -> IO ()
