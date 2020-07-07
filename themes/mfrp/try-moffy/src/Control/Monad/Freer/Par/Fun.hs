{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Freer.Par.Fun (
	Fun(..), Taggable(..), Tag(..), MaybeTag(..), Boolean(..) ) where

import Numeric.Natural

class Fun f where
	fun :: (a -> m b) -> f m a b
	($$) :: Applicative m => f m a b -> a -> m b

class Taggable (t :: (* -> *) -> * -> * -> *) where
	open :: Natural -> t m a a
	close :: Natural -> t m a a
	checkOpen :: t m a b -> t m a c -> MaybeTag a b c
	checkClose :: Tag -> t m a b -> Boolean a b

newtype Tag = Tag Natural deriving Show

data MaybeTag a b c where
	N :: MaybeTag a b c
	J :: Tag -> MaybeTag a a a

data Boolean a b where
	F :: Boolean a b
	T :: Boolean a a
