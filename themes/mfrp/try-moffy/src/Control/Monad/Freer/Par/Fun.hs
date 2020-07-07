{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Freer.Par.Fun (
	Fun(..), Taggable(..), MaybeId(..), Boolean(..), Id ) where

import Control.Monad.Freer.Par.Internal.Id

class Fun f where
	fun :: (a -> m b) -> f m a b
	($$) :: Applicative m => f m a b -> a -> m b

class Taggable (t :: (* -> *) -> * -> * -> *) where
	open :: Id -> t m a a
	close :: Id -> t m a a
	checkOpen :: t m a b -> t m a c -> MaybeId a b c
	checkClose :: Id -> t m a b -> Boolean a b

data MaybeId a b c where N :: MaybeId a b c; J :: Id -> MaybeId a a a
data Boolean a b where F :: Boolean a b; T :: Boolean a a
