{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.MaybeState (
	MaybeState(..),
	getHead, matchHead, checkHead_, checkHead, maybeHead ) where

import Control.Arrow
import Control.Monad
import Data.Bool

newtype MaybeState s a = MaybeState { runMaybeState :: s -> Maybe (a, s) }

instance Functor (MaybeState s) where
	f `fmap` MaybeState ms = MaybeState $ ((f `first`) <$>) . ms

instance Applicative (MaybeState s) where
	pure x = MaybeState $ Just . (x ,)
	MaybeState mf <*> MaybeState mx =
		MaybeState \s -> mf s >>= \(f, s') -> (f `first`) <$> mx s'

instance Monad (MaybeState s) where
	MaybeState mx >>= f =
		MaybeState \s -> mx s >>= \(x, s') -> f x `runMaybeState` s'

instance MonadFail (MaybeState s) where fail _ = MaybeState \_ -> Nothing

get :: MaybeState s s
get = MaybeState \s -> Just (s, s)

put :: s -> MaybeState s ()
put s = MaybeState \_ -> Just ((), s)

getHead :: MaybeState [a] a
getHead = get >>= \case [] -> fail ""; x : xs -> x <$ put xs

matchHead :: Eq a => a -> MaybeState [a] ()
matchHead = checkHead_ . (==)

checkHead_ :: (a -> Bool) -> MaybeState [a] ()
checkHead_ = void . checkHead

checkHead :: (a -> Bool) -> MaybeState [a] a
checkHead p = maybeHead $ bool Nothing <$> Just <*> p

maybeHead :: (a -> Maybe b) -> MaybeState [a] b
maybeHead p = maybe (fail "") pure . p =<< getHead
