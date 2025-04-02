{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module FreeCoyonedaMonads where

import FreeCoyoneda

data Reader e a where Reader :: Reader e e

ask :: Free (Coyoneda (Reader e)) e
ask = toFC Reader

runReader :: Free (Coyoneda (Reader e)) a -> e -> a
runReader m e = case m of
	Pure x -> x
	Join (Coyoneda Reader k) -> runReader (k e) e
