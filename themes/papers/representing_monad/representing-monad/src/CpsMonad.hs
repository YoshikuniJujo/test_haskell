{-# LANGUAGE RankNTypes, BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CpsMonad where

type CPS a = forall r . (a -> r) -> r

ret :: a -> CPS a
ret x = ($ x)

bind :: CPS a -> (a -> CPS b) -> CPS b
bind = ($)

type CPSState s a = forall r . (a -> s -> r) -> s -> r

ret' :: a -> CPSState s a
ret' x = ($ x)

bind' :: CPSState s a -> (a -> CPSState s b) -> CPSState s b
-- c `bind'` f = \k s -> c (\x s' k' -> f x k' s') s k
c `bind'` f = flip . c $ flip . f

get :: CPSState s s
get k s = k s s

put :: s -> CPSState s ()
put s k _ = k () s

runCPSState :: CPSState s a -> s -> (a, s)
runCPSState c = c ((,))
