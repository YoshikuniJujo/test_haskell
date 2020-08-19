{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Freer.Par.Funable (
	Funable(..), Taggable(..), Tag(..), sameTag, Id ) where

import Control.Monad.Freer.Par.Internal.Id

class Funable f where
	fun :: (a -> m b) -> f m a b
	($$) :: Applicative m => f m a b -> a -> m b

data Tag = NoTag | Tag Id deriving Show

sameTag :: Tag -> Tag -> Bool
l `sameTag` r | Tag i <- l, Tag j <- r = i == j | otherwise = False

class Taggable (t :: (* -> *) -> * -> * -> *) where
	putTag :: t m a b -> Id -> t m a b
	getTag :: t m a b -> Tag
