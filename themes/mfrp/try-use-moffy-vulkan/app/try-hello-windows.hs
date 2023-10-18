{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Prelude hiding (break)

import Data.OneOrMore (project)
import Data.OneOrMoreApp (pattern Singleton, expand)
import Data.Bool
import Control.Monad
import Control.Monad.Fix
import Control.Concurrent
import Control.Concurrent.STM hiding (retry)
import Control.Moffy
import Control.Moffy.Event.Gui
import Control.Moffy.Event.Window
import Control.Moffy.Handle hiding (expand)
import Control.Moffy.Handle.TChan
import Control.Moffy.Run.TChan

import ThreeWindows

main :: IO ()
main = runTryMultiWindow threeWindows

runHello :: IO (TChan (EvReqs GuiEv), TChan (EvOccs GuiEv), TChan ())
runHello = (,,) <$> newTChanIO <*> newTChanIO <*> newTChanIO >>= \cs@(crq, cocc, _cvw) ->
	cs <$ ( void . forkIO $ fix \loop -> do
		recieveEvReqs crq cocc
		threadDelay 500000
		loop )

recieveEvReqs :: TChan (EvReqs GuiEv) -> TChan (EvOccs GuiEv) -> IO ()
recieveEvReqs crq cocc = atomically (lastTChan crq) >>= \case
	Nothing -> pure ()
	Just rqs -> do
		case project rqs of
			Nothing -> pure ()
			Just WindowNewReq -> do
				putStrLn "recieve WindowNewReq"
				atomically . writeTChan cocc . expand . Singleton $ OccWindowNew (WindowId 0)

lastTChan :: TChan a -> STM (Maybe a)
lastTChan c = tryReadTChan c >>= maybe (pure Nothing)
	((isEmptyTChan c >>=) . bool (lastTChan c) . (pure . Just))

runTryMultiWindow :: Adjustable es GuiEv => Sig s es () r -> IO r
runTryMultiWindow s = do
	(cr, c, c') <- runHello
	interpret (retry $ handle (Just 0.5) cr c) c' s
