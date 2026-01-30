{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Monad
import Database.SmplstSQLite3

main :: IO ()
main = withSQLite "test.sqlite3" $ \db -> do
	(n, _) <- withPrepared db (
		"SELECT count(*) FROM sqlite_master " ++
		"WHERE type='table' and name='greeting'" ) $ \sm -> do
		_ <- step sm
		column sm 0 :: IO Int
	when (n == 0) $ do
		_ <- withPrepared db
			"CREATE TABLE greeting(id, words, greetee)" step
		return ()
	_ <- withPrepared db
		"INSERT INTO greeting VALUES(?, ?, 'world')" $ \sm -> do
		bindN sm 1 (155 :: Int)
		bindN sm 2 "good-bye"
		_ <- step sm
		reset sm
		bindN sm 1 (Just 222 :: Maybe Int)
		bindN sm 2 "hoge"
		print =<< step sm
		reset sm
		bindN sm 1 (Nothing :: Maybe Int)
		bindN sm 2 "foobar"
		print =<< step sm
	_ <- withPrepared db "SELECT * FROM greeting" $ \sm -> do
		print =<< step sm
		column sm 0 >>= (print :: Maybe Int -> IO ())
		column sm 1 >>= (print :: String -> IO ())
		column sm 2 >>= (print :: String -> IO ())
		print =<< step sm
		column sm 0 >>= (print :: Maybe Int -> IO ())
		column sm 1 >>= (print :: String -> IO ())
		column sm 2 >>= (print :: String -> IO ())
		print =<< step sm
		column sm 0 >>= (print :: Maybe Int -> IO ())
		column sm 1 >>= (print :: String -> IO ())
		column sm 2 >>= (print :: String -> IO ())
		print =<< step sm
	_ <- withPrepared db "DELETE FROM greeting" step
	return ()
