{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Examples where

-- import Database.HDBC ()
import Database.Record
import Database.Relational
import Database.Relational.Query.SQLite3
import Account

account_4_3_3a :: Relation () Account
account_4_3_3a = relation $ do
	a <- query account
	wheres $ a ! productCd' `in'` values ["CHK", "SAV", "CD", "MM"]
	return a

run :: (Show a, IConnection conn, FromSql SqlValue a, ToSql SqlValue p) =>
	conn -> p -> Relation p a -> IO ()
run conn param rel = do
	putStrLn $ "SQL: " ++ show rel
	records <- runRelation conn rel param
	mapM_ print records
	putStrLn ""
