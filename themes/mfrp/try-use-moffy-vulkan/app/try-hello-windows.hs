{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Prelude hiding (break)

import Data.Type.Set
import Data.Type.Flip
import Data.OneOrMore (project)
import Data.OneOrMoreApp (pattern Singleton, expand)
import Data.Bool
import Data.KeySym
import Control.Monad
import Control.Monad.Fix
import Control.Concurrent
import Control.Concurrent.STM hiding (retry)
import Control.Moffy
import Control.Moffy.Event.Gui
import Control.Moffy.Event.Window
import Control.Moffy.Event.Delete
import Control.Moffy.Event.Key
import Control.Moffy.Handle hiding (expand)
import Control.Moffy.Handle.TChan
import Control.Moffy.Run.TChan

main :: IO ()
main = runTryMultiWindow threeWindows

threeWindows :: Sig s GuiEv () ()
threeWindows = do
	w0 <- waitFor $ adjust windowNew
	w1 <- waitFor $ adjust windowNew
	w2 <- waitFor $ adjust windowNew
	_ <- (waitFor . find (\(x, y, z) -> not $ x || y || z) $ (,,) <$%> ddw w0 <*%> ddw w1 <*%> ddw w2) `break`
		(pressQ w0 `first` pressQ w1 `first` pressQ w2)

	pure ()

pressQ :: WindowId -> React s (Singleton KeyDown) ()
pressQ wid = bool (pressQ wid) (pure ()) . isQ =<< keyDown wid

ddw :: WindowId -> Sig s GuiEv Bool ()
ddw i = do
	emit True
	waitFor do
		adjust $ deleteEvent i
		adjust $ windowDestroy i
	emit False
	waitFor never

isQ :: KeySym -> Bool
isQ = \case Xk_q -> True; _ -> False

runHello :: IO (TChan (EvReqs GuiEv), TChan (EvOccs GuiEv), TChan ())
runHello = (,,) <$> newTChanIO <*> newTChanIO <*> newTChanIO >>= \cs@(crq, cocc, cvw) ->
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
