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

infixl 1 `bind'`

bind' :: CPSState s a -> (a -> CPSState s b) -> CPSState s b
-- c `bind'` f = \k s -> c (\x s' k' -> f x k' s') s k
c `bind'` f = flip . c $ flip . f

get :: CPSState s s
get k s = k s s

put :: s -> CPSState s ()
put s k _ = k () s

modify :: (s -> s) -> CPSState s ()
modify f = get `bind'` (put . f)

madd :: Num s => s -> CPSState s ()
madd n = modify (+ n)

mrecall :: Num s => CPSState s s
mrecall = get

runCPSState :: CPSState s a -> s -> (a, s)
runCPSState c = c ((,))

sample :: CPSState Int Int
sample =
	ret' (3 * 4) `bind'`
	madd `bind'` \_ ->
	ret' (2 * 5) `bind'`
	madd `bind'` \_ ->
	mrecall `bind'`
	ret' . (* 7)
