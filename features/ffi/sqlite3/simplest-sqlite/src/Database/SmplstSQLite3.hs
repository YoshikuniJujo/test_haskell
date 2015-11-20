{-# LANGUAGE TupleSections #-}

module Database.SmplstSQLite3 (
	-- * Functions
	withSQLite, withPrepared,
	step, reset, bind, bindN, column, SQLiteData, SQLiteDataList,
	-- * Types
	SQLite, Stmt, Result(..),
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

data Result = Busy | Row | Done deriving (Show, Eq)

foreign import ccall unsafe "sqlite3.h sqlite3_step" c_sqlite3_step ::
	Ptr Stmt -> IO CInt

step :: Stmt -> IO Result
step (Stmt psm) = do
	ret <- c_sqlite3_step psm
	case ret of
		_	| ret == sQLITE_BUSY -> return Busy
			| ret == sQLITE_ROW -> return Row
			| ret == sQLITE_DONE -> return Done
		_ -> ioError . userError $
			"Error while step: error code (" ++ show ret ++ ")"

foreign import ccall unsafe "sqlite3.h sqlite3_column_int"
	c_sqlite3_column_int :: Ptr Stmt -> CInt -> IO CInt
foreign import ccall unsafe "sqlite3.h sqlite3_column_text"
	c_sqlite3_column_text :: Ptr Stmt -> CInt -> IO CString

class SQLiteDataList a where
	bindNList :: Stmt -> Int -> [a] -> IO ()
	columnList :: Stmt -> Int -> IO [a]

class SQLiteData a where
	bindN :: Stmt -> Int -> a -> IO ()
	column :: Stmt -> Int -> IO a

instance SQLiteDataList a => SQLiteData [a] where
	bindN = bindNList
	column = columnList

instance SQLiteData Int where
	bindN = sqlite3BindInt
	column (Stmt sm) i =
		fromIntegral <$> c_sqlite3_column_int sm (fromIntegral i)

instance SQLiteDataList Char where
	bindNList = sqlite3BindString
	columnList (Stmt sm) i =
		peekCString =<< c_sqlite3_column_text sm (fromIntegral i)

{-
instance SQLiteData BS.ByteString where
	bindN = sqlite3BindByteString
	column (Stmt sm) i =
		BS.packCString =<< c_sqlite3_column_text sm (fromIntegral i)
		-}

instance SQLiteData T.Text where
	bindN sm i = sqlite3BindByteString sm i . T.encodeUtf8
	column (Stmt sm) i = T.decodeUtf8 <$>
		(BS.packCString =<< c_sqlite3_column_text sm (fromIntegral i))

foreign import ccall unsafe "sqlite3.h sqlite3_bind_int" c_sqlite3_bind_int ::
	Ptr Stmt -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "sqlite3.h sqlite3_bind_text" c_sqlite3_bind_text ::
	Ptr Stmt -> CInt -> CString -> Int -> Ptr a -> IO CInt

bind :: SQLiteData a => Stmt -> String -> a -> IO ()
bind sm ph x = flip (bindN sm) x =<< sqlite3BindParameterIndex sm ph

sqlite3BindInt :: Stmt -> Int -> Int -> IO ()
sqlite3BindInt (Stmt sm) i n = do
	ret <- c_sqlite3_bind_int sm (fromIntegral i) (fromIntegral n)
	when (ret /= sQLITE_OK) . ioError . userError $
			"Cannot bind int: error code (" ++ show ret ++ ")"

sqlite3BindString :: Stmt -> Int -> String -> IO ()
sqlite3BindString (Stmt sm) i s = withCString s $ \cs -> do
	ret <- c_sqlite3_bind_text sm (fromIntegral i) cs (- 1) nullPtr
	when (ret /= sQLITE_OK) . ioError . userError $
			"Cannot bind text: error code (" ++ show ret ++ ")"

sqlite3BindByteString :: Stmt -> Int -> BS.ByteString -> IO ()
sqlite3BindByteString (Stmt sm) i s = BS.useAsCString s $ \cs -> do
	ret <- c_sqlite3_bind_text sm (fromIntegral i) cs (- 1) nullPtr
	when (ret /= sQLITE_OK) . ioError . userError $
		"Cannot bind text: error code (" ++ show ret ++ ")"

foreign import ccall unsafe "sqlite3.h sqlite3_bind_parameter_index"
	c_sqlite3_bind_parameter_index ::
		Ptr Stmt -> CString -> IO CInt

sqlite3BindParameterIndex :: Stmt -> String -> IO Int
sqlite3BindParameterIndex (Stmt sm) ph = withCString ph $ \cph -> do
	i <- c_sqlite3_bind_parameter_index sm cph
	when (i == 0) . ioError . userError $
		"no such place holder: `" ++ ph ++ "'"
	return $ fromIntegral i

foreign import ccall unsafe "sqlite3.h sqlite3_reset" c_sqlite3_reset ::
	Ptr Stmt -> IO CInt

reset :: Stmt -> IO ()
reset (Stmt sm) = do
	ret <- c_sqlite3_reset sm
	when (ret /= sQLITE_OK) . ioError . userError $
		"Cannot reset stmt: error code (" ++ show ret ++ ")"
