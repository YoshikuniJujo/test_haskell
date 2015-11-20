#include <sqlite3.h>

module SQLite3.Constants (
	sQLITE_OK, sQLITE_ERROR, sQLITE_BUSY,
	sQLITE_ROW, sQLITE_DONE
	) where

import Foreign.C.Types

sQLITE_OK, sQLITE_ERROR, sQLITE_BUSY, sQLITE_ROW, sQLITE_DONE :: CInt
sQLITE_OK = #const SQLITE_OK
sQLITE_ERROR = #const SQLITE_ERROR
sQLITE_BUSY = #const SQLITE_BUSY
sQLITE_ROW = #const SQLITE_ROW
sQLITE_DONE = #const SQLITE_DONE
