{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Samples.Run.TChan where

import Control.Monad.Trans
import Control.Moffy
import Control.Moffy.Run (Handle, HandleSt, St)
import Control.Concurrent.STM

import qualified Control.Moffy.Run as M

interpret :: (Monad m, MonadIO m, Adjustable es es') =>
	Handle m es' -> TChan a -> Sig s es a r -> m r
interpret h c = M.interpret h (liftIO . atomically . writeTChan c)

interpretSt :: (Monad m, MonadIO m, Adjustable es es') =>
	HandleSt st m es' -> TChan a -> Sig s es a r -> St st m r
interpretSt h c = M.interpretSt h (liftIO . atomically . writeTChan c)
