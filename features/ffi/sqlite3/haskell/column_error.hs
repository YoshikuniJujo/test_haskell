import Database.SmplstSQLite3

main :: IO ()
main = withSQLite "hello.sqlite3" $ \db -> do
	_ <- withPrepared db "SELECT * FROM greeting" $ \sm -> do
		step sm
		column sm 0 >>= (print :: Int -> IO ())
		column sm 1 >>= (print :: String -> IO ())
		column sm 2 >>= (print :: String -> IO ())
	_ <- withPrepared db "SELECT * FROM greeting WHERE id = 100" $ \sm -> do
		step sm >>= print
		column sm 0 >>= (print :: Int -> IO ())
		column sm 1 >>= (print :: String -> IO ())
		column sm 2 >>= (print :: String -> IO ())
	return ()
