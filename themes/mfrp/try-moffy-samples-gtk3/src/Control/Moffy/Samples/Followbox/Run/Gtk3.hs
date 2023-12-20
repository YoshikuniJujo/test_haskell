{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Samples.Followbox.Run.Gtk3 (
	runFollowbox, runFollowbox' ) where

import Prelude hiding (break)

import Control.Arrow
import Control.Monad
import Control.Moffy
import Control.Moffy.Samples.Event.Delete
import Control.Moffy.Samples.Event.CalcTextExtents
import Control.Moffy.Samples.Handle.TChan
import Control.Moffy.Samples.Run.TChan
import Control.Concurrent
import Control.Concurrent.STM
import Data.Type.Set ((:-), (:+:), pattern Nil)
import Data.Type.Flip
import Data.Color
import System.Random

import Control.Moffy.Samples.Followbox.Event
import Control.Moffy.Samples.Followbox.Handle
import Control.Moffy.Samples.Followbox.TypeSynonym

import Control.Moffy.Samples.Run.Gtk3

import Control.Moffy.Samples.View

import Control.Moffy.Samples.Followbox.ViewType qualified as T
import Control.Moffy.Samples.Viewable.Text
import Control.Moffy.Samples.Viewable.Image
import Control.Moffy.Samples.Viewable.Shape
import Data.OneOfThem

import Data.Map qualified as M
import Control.Moffy.Samples.Event.Area qualified as A

runFollowbox :: String -> Sig s FollowboxEv T.View () -> IO ()
runFollowbox brws sig = runFollowbox_ brws Nothing . void
	$ viewToView <$%> sig `break` deleteEvent `break` checkTerminate

runFollowbox' :: String -> Sig s FollowboxEv ([(Int, Maybe Area)], T.View) () -> IO ()
runFollowbox' brws sig = runFollowbox_' brws Nothing $ (second viewToView) <$%> sig

runFollowbox_ :: String -> Maybe GithubNameToken -> Sig s FollowboxEv View () -> IO ()
runFollowbox_ brws tkn sig = do
	va <- atomically $ newTVar M.empty
	(cer, ceo, cv) <- atomically $
		(,,) <$> newTChan <*> newTChan <*> newTChan
	_ <- forkIO $ runFollowboxGen cer ceo va brws tkn cv (sig >> emit Stopped)
	runSingleWin cer ceo cv

runFollowbox_' :: String -> Maybe GithubNameToken -> Sig s FollowboxEv ([(Int, Maybe Area)], View) () -> IO ()
runFollowbox_' brws tkn sig = do
	va <- atomically $ newTVar M.empty
	(cer, ceo, cv) <- atomically $
		(,,) <$> newTChan <*> newTChan <*> newTChan
	_ <- forkIO $ runFollowboxGen' cer ceo va brws tkn cv (sig >> emit ([], Stopped))
	runSingleWin cer ceo cv

runFollowboxGen ::
	TChan (EvReqs (CalcTextExtents :- GuiEv)) -> TChan (EvOccs (CalcTextExtents :- GuiEv)) ->
	TVar (M.Map Int (A.Point, A.Point)) -> String ->
	Maybe GithubNameToken -> TChan x -> Sig s FollowboxEv x r -> IO r
runFollowboxGen cr c va brs mgnt c' s = do
	(r, _) <- interpretSt (handleFollowbox va (cr, c) brs mgnt) c' s (initialFollowboxState $ mkStdGen 8)
	pure r

type Area = (Point, Point)

runFollowboxGen' ::
	TChan (EvReqs (CalcTextExtents :- GuiEv)) -> TChan (EvOccs (CalcTextExtents :- GuiEv)) ->
	TVar (M.Map Int (A.Point, A.Point)) -> String ->
	Maybe GithubNameToken -> TChan x ->
	Sig s FollowboxEv ([(Int, Maybe Area)], x) r -> IO r
runFollowboxGen' cr c va brs mgnt c' s = do
	(r, _) <- interpretSt' (handleFollowbox va (cr, c) brs mgnt) va c' s (initialFollowboxState $ mkStdGen 8)
	pure r

handleFollowbox :: TVar (M.Map Int (A.Point, A.Point)) ->
	(TChan (EvReqs (CalcTextExtents :- GuiEv)), TChan (EvOccs (CalcTextExtents :- GuiEv))) -> Browser ->
	Maybe GithubNameToken -> HandleF IO (CalcTextExtents :- GuiEv :+: FollowboxEv)
handleFollowbox va f = handleFollowboxWith (uncurry . handle) f va

viewToView :: T.View -> View
viewToView (T.View vs) = View $ (view1ToView1 `apply`) <$> vs

view1ToView1 :: OneOfThemFun (VText :- Line :- Image :- 'Nil) View1
view1ToView1 =
	(\(Line' (T.Color r g b) lw p0 p1) ->
		VLine (RgbWord8 r g b) lw p0 p1) >--
	(\(T.Text' (T.Color r g b) fn fs p txt) ->
		VText (RgbWord8 r g b) fn fs p txt) >-- SingletonFun
	(\(T.Image' p (T.Png w h dt)) -> VImage p w h dt)
