{-# LANGUAGE TypeOperators #-}

import Data.Bool
import Control.Applicative

maybeGetLine :: IO (Maybe String)
maybeGetLine = (bool Nothing <$> Just <*> not . null) <$> getLine

fmap2 :: (Functor f, Functor g) => (a -> b) -> f (g a)  -> f (g b)
fmap2 = fmap . fmap

ap2 :: (Applicative f, Applicative g) => f (g (a -> b)) -> f (g a) -> f (g b)
ap2 = (<*>) . fmap (<*>)

newtype (f :+: g) x = Compose { decompose :: f (g x) } deriving Show

instance (Functor f, Functor g) => Functor (f :+: g) where
	fmap f = Compose . fmap2 f . decompose

instance (Applicative f, Applicative g) => Applicative (f :+: g) where
	pure = Compose . pure . pure
	(<*>) (Compose f) = Compose . ap2 f . decompose
