{-# LANGUAGE BlockArguments, MultilineStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Database.SmplstSQLite3

main :: IO ()
main = withSQLite "qiita.sqlite3" \db -> do
	(_, _) <- withPrepared db """
		CREATE TABLE users (
			id INTEGER PRIMARY KEY,
			name TEXT,
			age INTEGER
		);
		""" step
	pure ()
