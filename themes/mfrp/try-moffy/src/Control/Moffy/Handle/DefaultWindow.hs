{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Handle.DefaultWindow (
	DefaultWindowState(..), handleDefaultWindow ) where

import Control.Moffy.Event.Window
import Control.Moffy.Event.DefaultWindow
import Control.Moffy.Handle
import Data.Type.Set

import qualified Data.OneOrMore as Oom
import qualified Data.OneOrMoreApp as Ooma

class DefaultWindowState s where
	getDefaultWindow :: s -> Maybe WindowId
	putDefaultWindow :: s -> WindowId -> s

instance DefaultWindowState (Maybe WindowId) where
	getDefaultWindow = id
	putDefaultWindow _ = Just

handleDefaultWindow :: (DefaultWindowState s, Monad m) => HandleSt' s m DefaultWindowEv
handleDefaultWindow = handleStoreDefaultWindow `mergeSt` handleLoadDefaultWindow

handleStoreDefaultWindow :: (DefaultWindowState s, Applicative m) =>
	HandleSt' s m (Singleton StoreDefaultWindow)
handleStoreDefaultWindow (Oom.Singleton (StoreDefaultWindowReq wid)) s =
	pure (Just $ Ooma.Singleton (OccStoreDefaultWindow wid), s `putDefaultWindow` wid)

handleLoadDefaultWindow :: (DefaultWindowState s, Applicative m) =>
	HandleSt' s m (Singleton LoadDefaultWindow)
handleLoadDefaultWindow _rqs s = ($ getDefaultWindow s) $ pure . \case
	Nothing -> (Nothing, s)
	Just wid -> (Just . Ooma.Singleton $ OccLoadDefaultWindow wid, s)
