{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Arrow
import Control.Monad
import Control.Monad.State
import System.Environment
import System.Random qualified as R

import Tools

chars :: [Char]
chars = ['0' .. '9'] ++ ['A' .. 'Z'] ++ ['a' .. 'z'] ++
	['!' .. '/'] ++ [':' .. '@'] ++ ['[' .. '`'] ++
	['{' .. '~']

main :: IO ()
main = do
	[bs, n] <- getArgs
	(uncurry (writeFile . fileNameN bs "password") `mapM_`) . ([0 ..] `zip`)
		. fst $ replicateM (read n) password `runState` R.mkStdGen 8

select :: [a] -> State R.StdGen a
select xs = state $ ((xs !!) `first`) . R.randomR (0, length xs - 1)

randomR :: R.Random a => a -> a -> State R.StdGen a
randomR = curry (state . R.randomR)

password :: State R.StdGen String
password = do
	n <- randomR 5 30
	replicateM n (select chars)
