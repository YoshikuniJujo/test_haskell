{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array

newtype Sqlite3 = Sqlite3 (Ptr Sqlite3)
newtype InsertStmt = InsertStmt (Ptr InsertStmt)
newtype SelectStmt = SelectStmt (Ptr SelectStmt)

foreign import ccall unsafe "use_sqlite3.h test_open" c_open ::
	IO (Ptr Sqlite3)
foreign import ccall unsafe "use_sqlite3.h test_close" c_close ::
	Ptr Sqlite3 -> IO ()
foreign import ccall unsafe "sqlite3.h sqlite3_finalize" c_sqlite3Finalize ::
	Ptr a -> IO ()
foreign import ccall unsafe "use_sqlite3.h sample_table" c_sampleTable ::
	Ptr Sqlite3 -> IO ()
foreign import ccall unsafe "use_sqlite3.h insert_stmt" c_insertStmt ::
	Ptr Sqlite3 -> IO (Ptr InsertStmt)
foreign import ccall unsafe "use_sqlite3.h insert" c_insert ::
	Ptr Sqlite3 -> Ptr InsertStmt -> CString -> IO ()
foreign import ccall unsafe "use_sqlite3.h select_stmt" c_selectStmt ::
	Ptr Sqlite3 -> IO (Ptr SelectStmt)
foreign import ccall unsafe "use_sqlite3.h sql_select" c_select ::
	Ptr Sqlite3 -> Ptr SelectStmt -> CInt -> CInt -> CString -> IO ()

main :: IO ()
main = do
	conn <- c_open
	c_sampleTable conn
	istmt <- c_insertStmt conn
	newCString "test1" >>= c_insert conn istmt
	c_sqlite3Finalize istmt
	sstmt <- c_selectStmt conn
	allocaArray0 512 $ \ptr -> do
		c_select conn sstmt 1 512 ptr
		peekCString ptr >>= print
	c_sqlite3Finalize sstmt
	c_close conn
