{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Database.HDBC.Record

import Lib
import Correlated

main :: IO ()
main = do
	withConnection $ \conn ->
		print =<< runUpdate conn updatePersonByMoney2 ()
