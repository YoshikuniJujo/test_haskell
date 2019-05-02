{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds, DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Greeting where

-- import Database.Record.TH.SQLite3 (defineTable)

import Database.HDBC.Record
import Database.HDBC.Sqlite3
import Database.Relational

import Lib

-- $(defineTable "test.sqlite3" "greeting")
greeting2

selectAll :: IO [Greeting]
selectAll = do
	conn <- connectSqlite3 "test.sqlite3"
	runQuery' conn (relationalQuery Greeting.greeting) ()
