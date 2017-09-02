{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Monad.Cont

data VE e a = Val a | E (Reader e (VE e a))

newtype Reader e v = Reader (e -> v)

ask :: Cont (VE e a) e
ask = cont $ E . Reader

runReader :: Cont (VE e a) a -> e -> a
runReader m e = loop (runCont m Val) e

loop :: VE e a -> e -> a
loop m e = case m of
	Val x -> x
	E (Reader u) -> loop (u e) e
