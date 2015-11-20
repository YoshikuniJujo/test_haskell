module SQLite3 (SQLite3, withSQLite3) where

import Control.Monad
import Control.Exception
import System.IO.Error
import Foreign
import Foreign.C.Types
import Foreign.C.String

import SQLite3.Constants

data SQLite3 = SQLite3 (Ptr SQLite3) deriving Show

foreign import ccall unsafe "sqlite3.h sqlite3_open" c_sqlite3_open ::
	CString -> Ptr (Ptr SQLite3) -> IO CInt
foreign import ccall unsafe "sqlite3.h sqlite3_close" c_sqlite3_close ::
	Ptr SQLite3 -> IO CInt
foreign import ccall unsafe "sqlite3.h sqlite3_errmsg" c_sqlite3_errmsg ::
	Ptr SQLite3 -> IO CString

withSQLite3 :: String -> (SQLite3 -> IO a) -> IO a
withSQLite3 fp = bracket (sqlite3Open fp) sqlite3Close

sqlite3Open :: String -> IO SQLite3
sqlite3Open fp = withCString fp $ \cfp -> alloca $ \ppDb -> do
	ret <- c_sqlite3_open cfp ppDb
	db <- peek ppDb
	let sq = SQLite3 db
	when (ret /= sQLITE_OK) $ do
		em <- sqlite3Errmsg sq
		sqlite3Close sq
		ioError . userError $ "Cannot open database: " ++ em
	return sq

sqlite3Close :: SQLite3 -> IO ()
sqlite3Close sq@(SQLite3 db) = do
	ret <- c_sqlite3_close db
	when (ret /= sQLITE_OK) $ do
		em <- sqlite3Errmsg sq
		ioError . userError $ "Cannot close database: " ++ em

sqlite3Errmsg :: SQLite3 -> IO String
sqlite3Errmsg (SQLite3 db) = peekCString =<< c_sqlite3_errmsg db
