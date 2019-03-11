{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- create table persons (id int not null, can_buy int not null);
-- create table money (id int not null, pid int not null, money int not null);
-- create table money2 (pid int not null, money int not null);
-- insert into persons values (1, 0);
-- insert into persons values (2, 0);
-- insert into persons values (3, 0);
-- insert into money values (1, 1, 120);
-- insert into money values (2, 2, 90);
-- insert into money values (3, 3, 110);
-- insert into money2 values (1, 120);
-- insert into money2 values (2, 90);
-- insert into money2 values (3, 110);

-- % cat db_connect.info
-- 127.0.0.1
-- some_user
-- some_password
-- database
-- % chmod 600 db_connect.info

module Main where

import Database.Relational
import Database.HDBC.Record

import Lib
import Correlated

main :: IO ()
main = do
	putStrLn "table: persons (id int, can_buy int)"
	putStrLn "table: monies2 (pid int, money int)"
	putStrLn "table: monies (id int, pid int, money int)"
	putStrLn ""
	withConnection $ \conn -> do
		putStrLn "monies2"
		print =<< runQuery conn (relationalQuery showPersons) ()
		print =<< runQuery conn (relationalQuery showMonies2) ()
		print updatePersonByMoney2
		print =<< runUpdate conn updatePersonByMoney2 ()
		print =<< runQuery conn (relationalQuery showPersons) ()
		_ <- runUpdate conn resetPersons ()
		putStrLn ""
		putStrLn "monies"
		print =<< runQuery conn (relationalQuery showPersons) ()
		print =<< runQuery conn (relationalQuery showMonies) ()
		print updatePersonByMoney
		print =<< runUpdate conn updatePersonByMoney ()
		print =<< runQuery conn (relationalQuery showPersons) ()
		_ <- runUpdate conn resetPersons ()
		return ()
