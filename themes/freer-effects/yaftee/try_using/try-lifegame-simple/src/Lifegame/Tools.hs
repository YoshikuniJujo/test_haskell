{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lifegame.Tools where

div' :: Integral n => n -> n -> n
a `div'` b = (a - 1) `div` b + 1

times_ :: Monad m => Int -> m a -> m ()
times_ n act | n < 1 = pure () | otherwise = act >> times_ (n - 1) act
