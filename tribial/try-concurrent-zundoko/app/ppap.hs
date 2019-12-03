{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad.Tips (loopIf)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.RTQueue (dequeue)

import Zundoko (zundoko)

data PPAP = Pen | Pinapple | Apple | End deriving (Show, Eq)

say :: PPAP -> String
say = \case
	Pen -> "ペン"; Pinapple -> "パイナッポー"; Apple -> "アッポー";
	End -> ""

main :: IO ()
main = zundoko [Pen, Pinapple, Apple] [Pen, Pinapple, Apple, Pen] End >>= \q -> loopIf
	$ (<$) <$> (/= End) <*> putStrLn . say =<< atomically (dequeue q)
