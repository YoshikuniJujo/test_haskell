module Lib where

import Control.Concurrent.STM
import System.IO.Unsafe

globalVariable :: TVar String
globalVariable = unsafePerformIO . atomically $ newTVar "Hello, world!"
