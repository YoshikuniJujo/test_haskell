{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Monad.Cont
import Data.Monoid

data VE w v = V v | W (Writer w (VE w v))

data Writer w v = Writer w (() -> v)

tell :: w -> Cont (VE w v) ()
tell w = cont $ W . Writer w

runWriter :: Monoid w => Cont (VE w v) v -> (v, w)
runWriter m = wloop (runCont m V)

wloop :: Monoid w => VE w v -> (v, w)
wloop m = case m of
	V x -> (x, mempty)
	W (Writer w u) -> let (r, w') = wloop (u ()) in (r, w <> w')
