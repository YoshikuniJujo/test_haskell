{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Samples.Followbox.RunGtkField (runFollowbox) where

import Control.Concurrent
import Control.Moffy
import Control.Moffy.Samples.Event.CalcTextExtents
import Control.Moffy.Samples.Handle.TChan
import Control.Moffy.Samples.Run.TChan
import Control.Concurrent.STM
import Data.Type.Set ((:-), (:+:))
import System.Random

import Control.Moffy.Samples.Followbox.Event
import Control.Moffy.Samples.Followbox.Handle
import Control.Moffy.Samples.Followbox.TypeSynonym

import Control.Moffy.Samples.Run.Gtk4 hiding (GuiEv)

import Control.Moffy.Samples.View

runFollowbox :: String -> Maybe GithubNameToken -> Sig s FollowboxEv View () -> IO ()
runFollowbox brws tkn sig = do
	(cer, ceo, cv) <- atomically $
		(,,) <$> newTChan <*> newTChan <*> newTChan
	_ <- forkIO $ runFollowboxGen cer ceo brws tkn cv (sig >> emit Stopped)
	runSingleWin cer ceo cv

runFollowboxGen ::
	TChan (EvReqs (CalcTextExtents :- GuiEv)) -> TChan (EvOccs (CalcTextExtents :- GuiEv)) -> String ->
	Maybe GithubNameToken -> TChan x -> Sig s FollowboxEv x r -> IO r
runFollowboxGen cr c brs mgnt c' s = do
	(r, _) <- interpretSt (handleFollowbox (cr, c) brs mgnt) c' s (initialFollowboxState $ mkStdGen 8)
	pure r

handleFollowbox ::
	(TChan (EvReqs (CalcTextExtents :- GuiEv)), TChan (EvOccs (CalcTextExtents :- GuiEv))) -> Browser ->
	Maybe GithubNameToken -> HandleF IO (CalcTextExtents :- GuiEv :+: FollowboxEv)
handleFollowbox = handleFollowboxWith (uncurry . handle)
