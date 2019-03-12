{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Database.Relational
import Database.HDBC.Record

import Lib
import UseCityAge

main :: IO ()
main = do
	withConnection $ \conn -> do
		print =<< runQuery conn (relationalQuery getMaxAgeMembers) ()
