{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Followbox.RunGtk where

import Control.Moffy
import Control.Moffy.Event.CalcTextExtents
import Control.Moffy.Handle.GtkField hiding (GuiEv)
import Control.Moffy.Run
import Control.Concurrent.STM
import Data.Type.Set ((:-), (:+:))
import System.Random
import Graphics.Gtk

import Trial.Followbox.Event
import Trial.Followbox.Handle
import Trial.Followbox.GtkField
import Trial.Followbox.TypeSynonym

handleFollowbox ::
	(TChan (EvReqs (CalcTextExtents :- GuiEv)), TChan (EvOccs (CalcTextExtents :- GuiEv))) -> Browser ->
	Maybe GithubNameToken -> HandleF IO (CalcTextExtents :- GuiEv :+: FollowboxEv)
handleFollowbox = handleFollowboxWith (uncurry . handleDelete)

runFollowbox :: Browser -> Maybe GithubNameToken -> SigF s View r -> IO r
runFollowbox brs mgnt s = do
	(cr, c, c') <- tryUseTChanGen drawFollowboxGtk
	(r, _) <- interpretSt (handleFollowbox (cr, c) brs mgnt) (atomically . writeTChan c') s (initialFollowboxState $ mkStdGen 8)
	r <$ gtkMainQuit
