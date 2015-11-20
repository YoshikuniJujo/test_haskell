#include <sqlite3.h>

module SQLite3.Constants (
	sQLITE_OK
	) where

import Foreign.C.Types

sQLITE_OK :: CInt
sQLITE_OK = #const SQLITE_OK
