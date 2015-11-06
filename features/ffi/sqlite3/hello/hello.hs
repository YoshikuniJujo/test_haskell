{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign.Ptr

newtype Sqlite3 = Ptr Sqlite3

foreign import ccall unsafe "use_sqlite3.h test_open" c_open ::
	IO (Ptr Sqlite3)
foreign import ccall unsafe "use_sqlite3.h test_close" c_close ::
	Ptr Sqlite3 -> IO ()

main :: IO ()
main = do
	psql <- c_open
	c_close psql
