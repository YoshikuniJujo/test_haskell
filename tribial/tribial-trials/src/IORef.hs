module IORef where

import Data.IORef
import System.IO.Unsafe

foo, bar :: IORef [Int]
foo = unsafePerformIO $ newIORef []
bar = unsafePerformIO $ newIORef []
