{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tools where

import Control.Arrow
import Control.Monad
import Control.Monad.State
import Data.List.NonEmpty qualified as NE
import System.Random qualified as R

fileNameN :: String -> String -> Int -> FilePath
fileNameN bs ex n = bs ++ showInt2 n ++ "." ++ ex

showInt2 :: Int -> String
showInt2 n = replicate (2 - length s) '0' ++ s where s = show n

password :: State R.StdGen String
password = do
	n <- randomR 5 30
	replicateM n (select chars)

chars :: [Char]
chars = ['0' .. '9'] ++ ['A' .. 'Z'] ++ ['a' .. 'z'] ++
	['!' .. '/'] ++ [':' .. '@'] ++ ['[' .. '`'] ++
	['{' .. '~']

randomR :: R.Random a => a -> a -> State R.StdGen a
randomR = curry (state . R.randomR)

select :: [a] -> State R.StdGen a
select xs = state $ ((xs !!) `first`) . R.randomR (0, length xs - 1)

spanR :: (a -> Bool) -> [a] -> Either [a] (NE.NonEmpty a, [a])
spanR p = \case
	[] -> Left []
	x : xs -> case (p x, spanR p xs) of
		(_, Right td) -> Right $ (x NE.<|) `first` td
		(False, Left d) -> Right (x NE.:| [], d)
		(True, Left d) -> Left $ x : d

takeR :: Int -> [a] -> [a]
takeR n xs = take (length xs - n) xs
