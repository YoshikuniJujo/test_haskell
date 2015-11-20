{-# LANGUAGE TupleSections #-}

module Database.SmplstSQLite3 (
	-- * Functions
	withSQLite, withPrepared, step, SQLiteData, column, SQLiteDataList,
	-- * Types
	SQLite, Stmt, ResultCode(..),
	) where

import Control.Applicative
import Control.Monad
import Control.Exception
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Foreign
import Foreign.C.Types
import Foreign.C.String

import Database.SmplstSQLite3.Constants

data SQLite = SQLite (Ptr SQLite) deriving Show
data Stmt = Stmt (Ptr Stmt) deriving Show

foreign import ccall unsafe "sqlite3.h sqlite3_open" c_sqlite3_open ::
	CString -> Ptr (Ptr SQLite) -> IO CInt
foreign import ccall unsafe "sqlite3.h sqlite3_close" c_sqlite3_close ::
	Ptr SQLite -> IO CInt
foreign import ccall unsafe "sqlite3.h sqlite3_errmsg" c_sqlite3_errmsg ::
	Ptr SQLite -> IO CString

withSQLite :: String -> (SQLite -> IO a) -> IO a
withSQLite fp = bracket (sqlite3Open fp) sqlite3Close

sqlite3Open :: String -> IO SQLite
sqlite3Open fp = withCString fp $ \cfp -> alloca $ \ppDb -> do
	ret <- c_sqlite3_open cfp ppDb
	db <- peek ppDb
	let sq = SQLite db
	when (ret /= sQLITE_OK) $ do
		em <- sqlite3Errmsg sq
		sqlite3Close sq
		ioError . userError $ "Cannot open database: " ++ em
	return sq

sqlite3Close :: SQLite -> IO ()
sqlite3Close sq@(SQLite db) = do
	ret <- c_sqlite3_close db
	when (ret /= sQLITE_OK) $ do
		em <- sqlite3Errmsg sq
		ioError . userError $ "Cannot close database: " ++ em

sqlite3Errmsg :: SQLite -> IO String
sqlite3Errmsg (SQLite db) = peekCString =<< c_sqlite3_errmsg db

foreign import ccall unsafe "sqlite3.h sqlite3_prepare_v2"
	c_sqlite3_prepare_v2 ::
		Ptr SQLite -> CString -> CInt -> Ptr (Ptr Stmt) ->
		(Ptr CString) -> IO CInt
foreign import ccall unsafe "sqlite3.h sqlite3_finalize" c_sqlite3_finalize ::
	Ptr Stmt -> IO CInt

withPrepared :: SQLite -> String -> (Stmt -> IO a) -> IO (a, String)
withPrepared db sql act =
	bracket (sqlite3PrepareV2 db sql) (sqlite3Finalize . fst) $
		\(sm, t) -> (, t) <$> act sm

sqlite3PrepareV2 :: SQLite -> String -> IO (Stmt, String)
sqlite3PrepareV2 db@(SQLite pdb) sql =
	withCString sql $ \csql -> alloca $ \psm -> alloca $ \pt -> do
		ret <- c_sqlite3_prepare_v2 pdb csql (- 1) psm pt
		sm <- Stmt <$> peek psm
		when (ret /= sQLITE_OK) $ do
			em <- sqlite3Errmsg db
			sqlite3Finalize sm
			ioError . userError $ "Cannot prepare: " ++ em
		(sm ,) <$> (peekCString =<< peek pt)

sqlite3Finalize :: Stmt -> IO ()
sqlite3Finalize (Stmt psm) = do
	ret <- c_sqlite3_finalize psm
	when (ret /= sQLITE_OK) $
		ioError . userError $
			"Cannot finalize stmt: error code (" ++ show ret ++ ")"

data ResultCode = SQLiteBusy | SQLiteRow | SQLiteDone deriving (Show, Eq)

foreign import ccall unsafe "sqlite3.h sqlite3_step" c_sqlite3_step ::
	Ptr Stmt -> IO CInt

step :: Stmt -> IO ResultCode
step (Stmt psm) = do
	ret <- c_sqlite3_step psm
	case ret of
		_	| ret == sQLITE_BUSY -> return SQLiteBusy
			| ret == sQLITE_ROW -> return SQLiteRow
			| ret == sQLITE_DONE -> return SQLiteDone
		_ -> ioError . userError $
			"Error while step: error code (" ++ show ret ++ ")"

foreign import ccall unsafe "sqlite3.h sqlite3_column_int"
	c_sqlite3_column_int :: Ptr Stmt -> CInt -> IO CInt
foreign import ccall unsafe "sqlite3.h sqlite3_column_text"
	c_sqlite3_column_text :: Ptr Stmt -> CInt -> IO CString

class SQLiteDataList a where
	columnList :: Stmt -> Int -> IO [a]

class SQLiteData a where
	column :: Stmt -> Int -> IO a

instance SQLiteDataList a => SQLiteData [a] where
	column = columnList

instance SQLiteData Int where
	column (Stmt sm) i =
		fromIntegral <$> c_sqlite3_column_int sm (fromIntegral i)

instance SQLiteDataList Char where
	columnList (Stmt sm) i =
		peekCString =<< c_sqlite3_column_text sm (fromIntegral i)

instance SQLiteData BS.ByteString where
	column (Stmt sm) i =
		BS.packCString =<< c_sqlite3_column_text sm (fromIntegral i)

instance SQLiteData T.Text where
	column sm i = T.decodeUtf8 <$> column sm i
