{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Funable (Funable(..), Taggable(..), Id) where

import Internal.Id

class Funable f where
	fun :: (a -> m b) -> f m a b
	($$) :: Applicative m => f m a b -> a -> m b

class Taggable (t :: (* -> *) -> * -> * -> *) where
	putTag :: Id -> t m a b -> t m a b
	getTag :: t m a b -> Maybe Id
