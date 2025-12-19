{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lifegame.Tools where

div' :: Integral n => n -> n -> n
a `div'` b = (a - 1) `div` b + 1

times_ :: (Integral n, Monad m) => n -> m a -> m ()
n `times_` act | n < 1 = pure () | otherwise = act >> (n - 1) `times_` act
