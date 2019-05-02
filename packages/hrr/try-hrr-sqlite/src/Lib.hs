{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import GHC.Generics
import Data.Maybe
import Language.Haskell.TH
import Database.HDBC.Session
import Database.HDBC.Query.TH
import Database.HDBC.Sqlite3
import Database.HDBC.Schema.SQLite3
import Database.HDBC.Schema.Driver

import qualified Data.Map as Map

greeting :: Q [Dec]
greeting = defineTableFromDB
	(connectSqlite3 "test.sqlite3")
	driverSQLite3
	"main"
	"greeting"
	[''Show, ''Generic]

greeting2 :: Q [Dec]
greeting2 = do
	let	config = driverConfig drv
	(((cols, notNullIdxs), primaryCols), _logs) <- runIO getDbInfo
	let	cols1 = [ (,) cn . maybe ty (liftMaybe ty) . Map.lookup cn $ Map.fromList [] | (cn, ty) <- cols ]
		colIxMap = Map.fromList $ zip [c | (c, _) <- cols] [(0 :: Int) .. ]
		ixLookups = [ (k, Map.lookup k colIxMap) | k <- primaryCols ]
		primaryIxs = fromMaybe [] . sequence $ map snd ixLookups
	defineTableDefault config scm tbl cols1 [''Show, ''Generic]
		primaryIxs (listToMaybe notNullIdxs)
	where
	drv = driverSQLite3 :: Driver Connection
	scm = "main"
	tbl = "greeting"
	conn = connectSqlite3 "test.sqlite3"
	getDbInfo = do
		logChan <- emptyLogChan
		infoP <- withConnectionIO conn $ \c -> (,)
			<$> getFields drv c logChan scm tbl
			<*> getPrimaryKey drv c logChan scm tbl
		(,) infoP <$> takeLogs logChan
	liftMaybe tyQ sty = do
		ty <- tyQ
		case ty of
			(AppT (ConT n) _) | n == ''Maybe -> [t| Maybe $(sty) |]
			_ -> sty
