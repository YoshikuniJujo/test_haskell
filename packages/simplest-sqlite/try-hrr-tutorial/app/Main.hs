{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Database.SmplstSQLite3

main :: IO ()
main = withSQLite "test.sqlite3" $ \db -> do
	_ <- withPrepared db "CREATE TABLE greeting(id, words, greetee)" step
		>>= print
	_ <- withPrepared db "INSERT INTO greeting VALUES(?, ?, 'world')" $ \sm -> do
		bindN sm 1 (155 :: Int)
		bindN sm 2 "good-bye"
		_ <- step sm
		reset sm
		bindN sm 1 (222 :: Int)
		bindN sm 2 "hoge"
		step sm
	_ <- withPrepared db "SELECT * FROM greeting" $ \sm -> do
		_ <- step sm
		column sm 0 >>= print @Int
		column sm 1 >>= print @String
		column sm 2 >>= print @String
		_ <- step sm
		column sm 0 >>= print @Int
		column sm 1 >>= print @String
		column sm 2 >>= print @String
	return ()
