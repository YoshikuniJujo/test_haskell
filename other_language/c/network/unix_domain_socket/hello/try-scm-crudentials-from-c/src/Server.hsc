module Server where

import Data.Word
import Foreign.Ptr
import Foreign.C.Types

import Lib

#include <sys/socket.h>

foreign import ccall "bind" c_bind :: CInt -> Ptr SockaddrUn -> #type socklen_t
