{-# LANGUAGE BlockArguments, MultilineStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Database.SmplstSQLite3

main :: IO ()
main = withSQLite "foreignkey.sqlite3" \db -> do
	print =<< withPrepared db "DROP TABLE test" step
	print =<< withPrepared db "DROP TABLE test2" step
	print =<< withPrepared db """
		PRAGMA foreign_keys = true;
		""" step
	print =<< withPrepared db """
		CREATE TABLE test(
			id, -- int primary key,
			name varchar(32)
			);
		""" step
	print =<< withPrepared db """
		CREATE TABLE test2(
			id int primary key,
			name varchar(32),
			test_id int
			-- foreign key (test_id) references test(id)
			);
		""" step
	print =<< withPrepared db "INSERT INTO test VALUES (1, 'hoge')" step
	print =<< withPrepared db "INSERT INTO test VALUES (2, 'piyo')" step
	print =<< withPrepared db "INSERT INTO test VALUES (2, 'boo')" step
	print =<< withPrepared db "INSERT INTO test2 VALUES (1, 'test', 2)" step
	print =<< withPrepared db "SELECT * FROM test2" \stmt -> do
		step stmt
		print @Int =<< column stmt 0
		print @String =<< column stmt 1
		print @Int =<< column stmt 2
	print =<< withPrepared db """
		SELECT *
			FROM test2 AS t2
			JOIN test AS t
			ON t2.test_id == t.id
		""" \stmt -> do
		print =<< step stmt
		print @Int =<< column stmt 0
		print @String =<< column stmt 1
		print @Int =<< column stmt 2
		print @Int =<< column stmt 3
		print @String =<< column stmt 4
		print =<< step stmt
		print @Int =<< column stmt 0
		print @String =<< column stmt 1
		print @Int =<< column stmt 2
		print @Int =<< column stmt 3
		print @String =<< column stmt 4
		print =<< step stmt
