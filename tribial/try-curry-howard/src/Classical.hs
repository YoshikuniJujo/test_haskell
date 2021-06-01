{-# LANGUAGE LambdaCase, EmptyCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Classical where

import qualified Control.Exception as E
import Data.Typeable
import System.IO.Unsafe

data Void

type Not a = a -> Void

-- foo :: (forall b . (a -> b) -> b) -> a
-- foo f = f id

bar :: a -> ((a -> b) -> b)
bar x f = f x

data ExceptionValue a = ExceptionValue { unExceptionValue :: a }
	deriving (Show, Typeable)

instance (Show a, Typeable a) => E.Exception (ExceptionValue a)

throw :: (Show a, Typeable a) => a -> b
throw x = unsafePerformIO $ E.throw (ExceptionValue x)

-- catch :: (Show a, Typeable a) => ((a -> Void) -> Void) -> a
catch :: (Show a, Typeable a) => ((a -> b) -> Void) -> a
catch f = unsafePerformIO
	$ E.catch (E.evaluate . (\case) $ f throw) (pure . unExceptionValue)

doubleNegativeElimination :: (Show a, Typeable a) => Not (Not a) -> a
doubleNegativeElimination = catch

baz :: Not (Not Int)
baz f = f 123

hoge :: Int
hoge = doubleNegativeElimination baz
