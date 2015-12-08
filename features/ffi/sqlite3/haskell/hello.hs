{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad
import qualified Data.ByteString as BS
import Database.SmplstSQLite3

main :: IO ()
main = withSQLite "hello.sqlite3" $ \db -> do
--	_ <- withPrepared db
--		"CREATE TABLE greeting(id, words, greetee)" step
	_ <- withPrepared db "DELETE FROM GREETING" step
	_ <- withPrepared db
		"INSERT INTO greeting VALUES (:id, :words, :greetee)" $ \sm -> do
		bind sm ":id" (123 :: Int)
		bind sm ":words" ("hello" :: String)
		bind sm ":greetee" ("you" :: String)
		step sm
		reset sm
		bind sm ":id" (456 :: Int)
		bind sm ":words" ("good-bye" :: String)
		bind sm ":greetee" ("you" :: String)
		step sm
		reset sm
		bind sm ":id" (888.8 :: Double)
		bind sm ":words" ()
		bind sm ":greetee"  ("blob" :: BS.ByteString)
		step sm
	withPrepared db "SELECT * FROM greeting" $ \sm -> doWhile_ $ do
		r <- step sm
		if r == Row
			then do mapM_ (printColumn sm) [0 .. 2]
				return True
			else return False
	return ()

doWhile_ :: Monad m => m Bool -> m ()
doWhile_ act = do
	b <- act
	when b $ doWhile_ act

doWhile :: (Monad m, Functor m) => m (Maybe a) -> m [a]
doWhile act = do
	r <- act
	maybe (return []) ((<$> doWhile act) . (:)) r

printColumn :: Stmt -> Int -> IO ()
printColumn sm i = do
	t <- columnType sm i
	putStr $ show t ++ ": "
	case t of
		Integer -> (column sm i :: IO Int) >>= print
		Float -> (column sm i :: IO String) >>= print
		Text -> (column sm i :: IO String) >>= print
		Blob -> (column sm i :: IO BS.ByteString) >>= print
		_ -> (column sm i :: IO ()) >>= print
