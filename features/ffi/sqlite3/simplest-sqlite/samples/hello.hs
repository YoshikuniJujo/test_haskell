import Database.SmplstSQLite3

main :: IO ()
main = withSQLite "test.sqlite3" $ \db -> do
--	withPrepared db "CREATE TABLE greeting(id, words, greetee)" step >>= print
	withPrepared db
		"INSERT INTO greeting VALUES(?, ?, 'world')" $ \sm -> do
		bindN sm 1 (155 :: Int)
		bindN sm 2 "good-bye"
		step sm
		reset sm
		bindN sm 1 (222 :: Int)
		bindN sm 2 "hoge"
		step sm
	withPrepared db "SELECT * FROM greeting" $ \sm -> do
--		step sm
		column sm 0 >>= (print :: Int -> IO ())
		column sm 1 >>= (print :: String -> IO ())
		column sm 2 >>= (print :: String -> IO ())
		step sm
		column sm 0 >>= (print :: Int -> IO ())
		column sm 1 >>= (print :: String -> IO ())
		column sm 2 >>= (print :: String -> IO ())
	return ()
