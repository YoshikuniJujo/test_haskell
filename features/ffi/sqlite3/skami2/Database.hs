{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings #-}

module Database (
	SQLite, Stmt,
	open, close, newTable, bindStmt, runStmt, getSaltHash, mkAccount,
	finalizeStmt,
	) where

import Control.Applicative
import Control.Exception
import qualified Data.ByteString as BS
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array

newtype SQLite = SQLite (Ptr SQLite) deriving Show
newtype Stmt = Stmt (Ptr Stmt) deriving Show

foreign import ccall unsafe "use_sqlite3.h sql_open" c_open ::
	IO (Ptr SQLite)
foreign import ccall unsafe "use_sqlite3.h sql_close" c_close ::
	Ptr SQLite -> IO ()
foreign import ccall unsafe "sqlite3.h sqlite3_finalize" c_sqlite3Finalize ::
	Ptr a -> IO ()
foreign import ccall unsafe "use_sqlite3.h mk_stmt" c_mkStmt ::
	Ptr SQLite -> CString -> IO (Ptr Stmt)
foreign import ccall unsafe "use_sqlite3.h bind_stmt" c_bindStmt ::
	Ptr Stmt -> CString -> CString -> IO ()
foreign import ccall unsafe "use_sqlite3.h run_stmt" c_runStmt ::
	Ptr Stmt -> IO ()
foreign import ccall unsafe "use_sqlite3.h get_stmt" c_getStmt ::
	Ptr Stmt -> CInt -> CString -> IO ()
foreign import ccall unsafe "use_sqlite3.h get2_stmt" c_get2Stmt ::
	Ptr Stmt -> CInt -> CString -> CString -> IO ()

open :: IO SQLite
open = SQLite <$> c_open

close :: SQLite -> IO ()
close (SQLite conn) = c_close conn

finalizeStmt :: Stmt -> IO ()
finalizeStmt (Stmt stmt) = c_sqlite3Finalize stmt

withStmt :: SQLite -> BS.ByteString -> (Stmt -> IO a) -> IO a
withStmt (SQLite conn) bs f = BS.useAsCString bs $ \cs -> do
	stmt <- c_mkStmt conn cs
	x <- f $ Stmt stmt
	c_sqlite3Finalize stmt
	return x

mkStmt :: SQLite -> BS.ByteString -> IO Stmt
mkStmt (SQLite conn) bs = (Stmt <$>) . BS.useAsCString bs $ c_mkStmt conn

bindStmt :: Stmt -> BS.ByteString -> BS.ByteString -> IO ()
bindStmt (Stmt stmt) ph val = BS.useAsCString (":" `BS.append` ph) $ \cph ->
	BS.useAsCString val $ c_bindStmt stmt cph

runStmt :: Stmt -> IO ()
runStmt (Stmt stmt) = c_runStmt stmt

getStmt :: Stmt -> IO BS.ByteString
getStmt (Stmt stmt) = allocaArray0 512 $ \ptr -> do
	c_getStmt stmt 512 ptr
	BS.packCString ptr

get2Stmt :: Stmt -> IO (BS.ByteString, BS.ByteString)
get2Stmt (Stmt stmt) = allocaArray0 512 $ \ptr1 -> do
	allocaArray0 512 $ \ptr2 -> do
		c_get2Stmt stmt 512 ptr1 ptr2
		(,) <$> BS.packCString ptr1 <*> BS.packCString ptr2

stmtNewTable :: BS.ByteString
stmtNewTable = "CREATE TABLE account (" `BS.append`
	"name PRIMARY KEY, " `BS.append`
	"salt, hash, mail_address, activated)"

newTable :: SQLite -> IO ()
newTable conn = bracket (mkStmt conn stmtNewTable) finalizeStmt runStmt

stmtSaltHash :: BS.ByteString
stmtSaltHash = "SELECT salt, hash FROM account WHERE name = :name"

getSaltHash :: SQLite -> BS.ByteString -> IO (BS.ByteString, BS.ByteString)
getSaltHash conn nm = (\act -> bracket act finalizeStmt get2Stmt) $ do
	stmt <- mkStmt conn stmtSaltHash
	bindStmt stmt "name" nm
	return stmt

stmtMkAccount :: BS.ByteString
stmtMkAccount = "INSERT INTO account (" `BS.append`
	"name, salt, hash, mail_address, activated) VALUES (" `BS.append`
	":name, :salt, :hash, :mail_address, 0)"

mkAccount :: SQLite -> IO Stmt
mkAccount conn = mkStmt conn stmtMkAccount
