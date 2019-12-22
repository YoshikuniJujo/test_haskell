module Tips where

import Control.Monad
import Control.Concurrent
import Data.Bool

forkForever :: IO a -> IO ()
forkForever = void . forkIO . forever

loopIf :: Monad m => m Bool -> m ()
loopIf act = bool (return ()) (loopIf act) =<< act
