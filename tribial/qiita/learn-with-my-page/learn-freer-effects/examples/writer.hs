{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Arrow
-- import Data.Monoid

import FreeCoyoneda

data Writer w a where
	Writer :: w -> Writer w ()

tell :: w -> Free (Coyoneda (Writer w)) ()
tell = toFC . Writer

runWriter :: Monoid w => Free (Coyoneda (Writer w)) a -> (a, w)
runWriter = \case
	Pure x -> (x, mempty)
	Join (Coyoneda (Writer w) k) -> second (w <>) . runWriter $ k ()

sample :: Free (Coyoneda (Writer String)) Integer
sample = do
	tell "x = 8\n"
	x <- return 8
	tell "y = 5\n"
	y <- return 5
	tell $ "x + y = " ++ show (x * y) ++ "\n"
	return $ x * y
