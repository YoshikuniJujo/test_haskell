{-# LANGUAGE ForeignFunctionInterface #-}

import Control.Applicative
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array
import Data.Time

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

open :: IO Sqlite3
open = Sqlite3 <$> c_open

close :: Sqlite3 -> IO ()
close (Sqlite3 p) = c_close p

sampleTable :: Sqlite3 -> IO ()
sampleTable (Sqlite3 p) = c_sampleTable p

withInsertStmt :: Sqlite3 -> (InsertStmt -> IO a) -> IO a
withInsertStmt (Sqlite3 p1) f = do
	p2 <- c_insertStmt p1
	x <- f $ InsertStmt p2
	c_sqlite3Finalize p2
	return x

insert :: Sqlite3 -> InsertStmt -> String -> IO ()
insert (Sqlite3 p1) (InsertStmt p2) s = newCString s >>= c_insert p1 p2

withSelectStmt :: Sqlite3 -> (SelectStmt -> IO a) -> IO a
withSelectStmt (Sqlite3 p1) f = do
	p2 <- c_selectStmt p1
	x <- f $ SelectStmt p2
	c_sqlite3Finalize p2
	return x

select :: Sqlite3 -> SelectStmt -> Int -> IO String
select (Sqlite3 p1) (SelectStmt p2) i = allocaArray0 512 $ \ptr -> do
	c_select p1 p2 (fromIntegral i) 512 ptr
	peekCString ptr


main :: IO ()
main = do
	conn <- open
	sampleTable conn
	withInsertStmt conn $ \istmt ->
		show <$> getCurrentTime >>= insert conn istmt
	withSelectStmt conn $ \sstmt ->
		select conn sstmt 1 >>= print
	close conn
