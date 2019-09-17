{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Arrow
-- import Data.Monoid

import FreeCoyoneda

data RW e w a where
	Reader :: RW e w e
	Writer :: w -> RW e w ()

ask :: Free (Coyoneda (RW e w)) e
ask = toFC Reader

tell :: w -> Free (Coyoneda (RW e w)) ()
tell = toFC . Writer

sample :: Free (Coyoneda (RW String String)) (String, String)
sample = do
	x <- ask
	tell $ "I say " ++ x ++ ".\n"
	y <- ask
	tell $ "You say Good-bye!\n"
	return (x, y)

runRW :: Monoid w => Free (Coyoneda (RW e w)) a -> e -> (a, w)
runRW m e = case m of
	Pure x -> (x, mempty)
	Join (Coyoneda Reader k) -> runRW (k e) e
	Join (Coyoneda (Writer w) k) -> second (w <>) $ runRW (k ()) e

runStateRW :: Free (Coyoneda (RW s s)) a -> s -> (a, s)
runStateRW m s = case m of
	Pure x -> (x, s)
	Join (Coyoneda Reader k) -> runStateRW (k s) s
	Join (Coyoneda (Writer s') k) -> runStateRW (k ()) s'
