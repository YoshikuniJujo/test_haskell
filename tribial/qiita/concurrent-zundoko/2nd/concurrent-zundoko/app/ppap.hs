{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Concurrent.STM
import Data.Maybe

import Tips
import RealtimeQueueStm
import ZunDoko

data PPAP = Pen | Pineapple | Apple deriving (Show, Eq)

say :: PPAP -> String
say = \case Pen -> "ペン"; Pineapple -> "パイナッポー"; Apple -> "アッポー"

main :: IO ()
main = zundoko [Pen, Pineapple, Apple] [Pen, Pineapple, Apple, Pen] >>= \q ->
	loopIf $ (<$) <$> isJust <*> maybe (return ()) (putStrLn . say)
		=<< atomically (dequeue q)
