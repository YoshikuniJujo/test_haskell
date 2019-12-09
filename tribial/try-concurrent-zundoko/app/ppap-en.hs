{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad.Tips (loopIf)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.RTQueue (dequeue)
import Data.Maybe (isJust)

import ZunDoko (zundoko)

data PPAP = Pen | Pinapple | Apple deriving (Show, Eq)

say :: PPAP -> String
say = \case Pen -> "pen"; Pinapple -> "pinapple"; Apple -> "apple"

main :: IO ()
main = zundoko [Pen, Pinapple, Apple] [Pen, Pinapple, Apple, Pen] >>= \q ->
	loopIf $ (<$) <$> isJust <*> maybe (return ()) (putStrLn . say)
		=<< atomically (dequeue q)
