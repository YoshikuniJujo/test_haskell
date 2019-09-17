{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Arrow
-- import Data.Monoid

import Freer

data Writer w a where
	Writer :: w -> Writer w ()

tell :: w -> Freer (Writer w) ()
tell = freer . Writer

runWriter :: Monoid w => Freer (Writer w) a -> (a, w)
runWriter = \case
	Pure x -> (x, mempty)
	Writer w `Bind` k -> second (w <>) . runWriter $ k ()

set :: String -> Integer -> Freer (Writer String) Integer
set var val = do
	tell $ var ++ " = " ++ show val ++ "\n"
	return val

sample :: Freer (Writer String) Integer
sample = do
	x <- set "x" 8
	y <- set "y" 5
	tell $ "x * y = " ++ show (x * y) ++ "\n"
	return $ x * y
