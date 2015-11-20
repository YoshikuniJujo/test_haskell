{-# LANGUAGE TupleSections #-}

module SQLite3 (SQLite3, withSQLite3, withPrepared, step, column) where

import Control.Applicative
import Control.Monad
import Control.Exception
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Foreign
import Foreign.C.Types
import Foreign.C.String

import SQLite3.Constants

data SQLite3 = SQLite3 (Ptr SQLite3) deriving Show
data SQLite3Stmt = SQLite3Stmt (Ptr SQLite3Stmt) deriving Show

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

foreign import ccall unsafe "sqlite3.h sqlite3_prepare_v2"
	c_sqlite3_prepare_v2 ::
		Ptr SQLite3 -> CString -> CInt -> Ptr (Ptr SQLite3Stmt) ->
		(Ptr CString) -> IO CInt
foreign import ccall unsafe "sqlite3.h sqlite3_finalize" c_sqlite3_finalize ::
	Ptr SQLite3Stmt -> IO CInt

withPrepared :: SQLite3 -> String -> (SQLite3Stmt -> IO a) -> IO (a, String)
withPrepared db sql act =
	bracket (sqlite3PrepareV2 db sql) (sqlite3Finalize . fst) $
		\(sm, t) -> (, t) <$> act sm

sqlite3PrepareV2 :: SQLite3 -> String -> IO (SQLite3Stmt, String)
sqlite3PrepareV2 db@(SQLite3 pdb) sql =
	withCString sql $ \csql -> alloca $ \psm -> alloca $ \pt -> do
		ret <- c_sqlite3_prepare_v2 pdb csql (- 1) psm pt
		sm <- SQLite3Stmt <$> peek psm
		when (ret /= sQLITE_OK) $ do
			em <- sqlite3Errmsg db
			sqlite3Finalize sm
			ioError . userError $ "Cannot prepare: " ++ em
		(sm ,) <$> (peekCString =<< peek pt)

sqlite3Finalize :: SQLite3Stmt -> IO ()
sqlite3Finalize (SQLite3Stmt psm) = do
	ret <- c_sqlite3_finalize psm
	when (ret /= sQLITE_OK) $
		ioError . userError $
			"Cannot finalize stmt: error code (" ++ show ret ++ ")"

data ResultCode = SQLiteBusy | SQLiteRow | SQLiteDone deriving Show

foreign import ccall unsafe "sqlite3.h sqlite3_step" c_sqlite3_step ::
	Ptr SQLite3Stmt -> IO CInt

step :: SQLite3Stmt -> IO ResultCode
step (SQLite3Stmt psm) = do
	ret <- c_sqlite3_step psm
	case ret of
		_	| ret == sQLITE_BUSY -> return SQLiteBusy
			| ret == sQLITE_ROW -> return SQLiteRow
			| ret == sQLITE_DONE -> return SQLiteDone
		_ -> ioError . userError $
			"Error while step: error code (" ++ show ret ++ ")"

foreign import ccall unsafe "sqlite3.h sqlite3_column_int"
	c_sqlite3_column_int :: Ptr SQLite3Stmt -> CInt -> IO CInt
foreign import ccall unsafe "sqlite3.h sqlite3_column_text"
	c_sqlite3_column_text :: Ptr SQLite3Stmt -> CInt -> IO CString

class SQLite3DataList a where
	columnList :: SQLite3Stmt -> Int -> IO [a]

class SQLite3Data a where
	column :: SQLite3Stmt -> Int -> IO a

instance SQLite3DataList a => SQLite3Data [a] where
	column = columnList

instance SQLite3Data Int where
	column (SQLite3Stmt sm) i =
		fromIntegral <$> c_sqlite3_column_int sm (fromIntegral i)

instance SQLite3DataList Char where
	columnList (SQLite3Stmt sm) i =
		peekCString =<< c_sqlite3_column_text sm (fromIntegral i)

instance SQLite3Data BS.ByteString where
	column (SQLite3Stmt sm) i =
		BS.packCString =<< c_sqlite3_column_text sm (fromIntegral i)

instance SQLite3Data T.Text where
	column sm i = T.decodeUtf8 <$> column sm i
