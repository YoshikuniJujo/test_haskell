{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import GHC.Generics
import Language.Haskell.TH
import Database.HDBC.Query.TH
import Database.HDBC.Sqlite3
import Database.HDBC.Schema.SQLite3

import Database.HDBC.Schema.Driver

greeting :: Q [Dec]
greeting = defineTableFromDB
	(connectSqlite3 "test.sqlite3")
	driverSQLite3
	"main"
	"greeting"
	[''Show, ''Generic]

greeting2 :: Q [Dec]
greeting2 = do
	let	config = driverConfig (driverSQLite3 :: Driver Connection)
	defineTableDefault config undefined undefined undefined undefined
		undefined undefined
