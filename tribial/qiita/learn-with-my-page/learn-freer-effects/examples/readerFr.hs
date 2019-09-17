{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Freer

data Reader e a where
	Reader :: Reader e e

ask :: Freer (Reader e) e
ask = freer Reader

runReader :: Freer (Reader e) a -> e -> a
runReader m e = case m of
	Pure x -> x
	Reader `Bind` k -> runReader (k e) e
