{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Database.Relational
import Database.HDBC.Record

import Lib
import Correlated

main :: IO ()
main = do
	withConnection $ \conn -> do
		print =<< runUpdate conn updatePersonByMoney2 ()
		runQuery conn (relationalQuery showPersons) () >>= print
		_ <- runUpdate conn resetPersons ()
		print =<< runUpdate conn updatePersonByMoney ()
		runQuery conn (relationalQuery showPersons) () >>= print
		_ <- runUpdate conn resetPersons ()
		return ()
