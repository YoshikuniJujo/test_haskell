{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Prelude hiding (break)

import Control.Monad
import Control.Moffy

import UseCairo qualified as Vk
import KeyToXKey

import Control.Concurrent
import Control.Concurrent.STM hiding (retry)
import Data.Default
import Data.List.Length
import Data.Map qualified as M
import Gpu.Vulkan.Cglm qualified as Cglm
import Gpu.Vulkan qualified as Vk
import Graphics.UI.GlfwG.Key qualified as GlfwG.Ky
import Graphics.UI.GlfwG.Mouse qualified as GlfwG.Ms

import Control.Moffy.Event.Window
import Control.Moffy.Event.DefaultWindow
import Control.Moffy.Event.Delete hiding (deleteEvent)
import Control.Moffy.Event.Key
import Control.Moffy.Event.Mouse (
	pattern OccMouseDown, pattern OccMouseUp, pattern OccMouseMove, MouseBtn(..))
import Data.OneOrMore (project)
import Data.OneOrMoreApp qualified as App (pattern Singleton, expand)

import Trial.Followbox
import Trial.Followbox.ViewType
import Trial.Followbox.RunGtkField (handleFollowbox)

import Control.Moffy.Event.Gui

import Control.Moffy.Event.Cursor
import Data.Type.Flip

import Trial.Followbox.Event
import Trial.Followbox.Handle hiding (GuiEv)

import Control.Moffy.Run.TChan
import System.Random
import Control.Moffy.Handle (ExpandableOccurred)
import Data.Type.Set

----------------------------------------------------------------------
--
-- * MAIN
-- * SEND REQUEST/EVENT TO VULKAN/MOFFY
-- * RUN FOLLOWBOX
-- * RECTANGLES
--
----------------------------------------------------------------------

-- MAIN

main :: IO ()
main = do
	(ccmd@(writeTChan -> cmd), cev@(readTChan -> ev), cex@(luex -> ex)) <-
		atomically $ (,,) <$> newTChan <*> newTChan <*> newTVar M.empty
	ff $ threadDelay 20000 >> atomically (cmd Vk.GetEvent)
	(crqs@(readTChan -> rqs), cocc@(writeTChan -> occ))
		<- atomically $ (,) <$> newTChan <*> newTChan
	ffa $ mffReqsToVk cmd occ =<< rqs; ffa $ vkEvToMoffy cmd occ ex =<< ev
	cviews@(readTChan -> views) <- atomically newTChan
	ffa $ views >>= \vs -> ex 0 >>= \e0 -> do
		cmd $ Vk.SetPicture (vs M.! WindowId 0)
		cmd $ Vk.Draw (M.fromList [(0, (def, rects 1024 1024 e0))])
	f $ interpretSt (handleFollowbox (crqs, cocc) "firefox" Nothing) cviews
		runFollowbox (initialFollowboxState $ mkStdGen 8)
	Vk.rectangles ccmd cev cex
	where
	luex mp k = maybe (pure (Vk.Extent2d 0 0))
		readTVar . M.lookup k =<< readTVar mp
	f = void . forkIO . void
	ff = void . forkIO . forever; ffa = ff . atomically

-- SEND REQUEST/EVENT TO VULKAN/MOFFY

mffReqsToVk :: (Vk.Command Int -> STM ()) ->
	(EvOccs GuiEv -> STM ()) -> EvReqs GuiEv -> STM ()
mffReqsToVk cmd occ rqs = do
	maybeWhen (project rqs) \WindowNewReq -> cmd Vk.OpenWindow
	maybeWhen (project rqs) \(WindowDestroyReq i@(WindowId k)) -> do
		cmd . Vk.DestroyWindow $ fromIntegral k
		occ . App.expand . App.Singleton $ OccWindowDestroy i
	maybeWhen (project rqs) \(SetCursorFromNameReq wid nc) ->
		occ . App.expand . App.Singleton
			$ OccSetCursorFromName wid nc Success
	maybeWhen (project rqs) \r -> cmd $ Vk.CalcTextLayoutExtent r
	where maybeWhen = maybe (const $ pure ()) (flip ($))

vkEvToMoffy ::
	(Vk.Command Int -> STM ()) -> (EvOccs GuiEv -> STM ()) ->
	(Int -> STM Vk.Extent2d) -> Vk.Event Int -> STM ()
vkEvToMoffy cmd occ ext = \case
	Vk.EventEnd -> pure ()
	Vk.EventOpenWindow (w -> i) -> o (OccWindowNew i)
	Vk.EventDeleteWindow (w -> i) -> o (OccDeleteEvent i)
	Vk.EventKeyDown (w -> i) GlfwG.Ky.Key'D -> o (OccDeleteEvent i)
	Vk.EventKeyDown (w -> i) ky -> o (OccKeyDown i $ keyToXKey ky)
	Vk.EventKeyUp (w -> i) ky -> o (OccKeyUp i $ keyToXKey ky)
	Vk.EventMouseButtonDown (w -> i) (bt -> b) -> o (OccMouseDown i b)
	Vk.EventMouseButtonUp (w -> i) (bt -> b) -> o (OccMouseUp i b)
	Vk.EventCursorPosition (w -> i) x y -> o (OccMouseMove i (x, y))
	Vk.EventTextLayoutExtentResult ex -> o ex
	Vk.EventNeedRedraw -> ext 0 >>= \e0 ->
		cmd $ Vk.Draw (M.fromList [(0, (def, rects 1024 1024 e0))])
	where
	o :: ExpandableOccurred (Singleton a) GuiEv => Occurred a -> STM ()
	o = occ . App.expand . App.Singleton
	w = WindowId . fromIntegral
	bt = \case
		GlfwG.Ms.MouseButton'1 -> ButtonLeft
		GlfwG.Ms.MouseButton'2 -> ButtonRight
		GlfwG.Ms.MouseButton'3 -> ButtonMiddle
		_ -> ButtonUnknown maxBound

-- RUN FOLLOWBOX

runFollowbox :: Sig s
	(CursorEv :+: StoreDefaultWindow :- FollowboxEv)
	(M.Map WindowId View) ()
runFollowbox = do
	i <- waitFor $ adjust windowNew
	waitFor do
		_ <- adjust $ setCursorFromName i Default
		adjust $ storeDefaultWindow i
	M.singleton i <$%> adjustSig (followbox i)
	waitFor . adjust $ windowDestroy i

-- RECTANGLES

rects :: Float -> Float -> Vk.Extent2d -> [Vk.Rectangle]
rects w h ex = [
	Vk.Rectangle (Vk.rectPos (- 2) (- 2)) (Vk.rectSize 4 4)
		(Vk.rectColor 1.0 0.0 0.0 1.0) m,
	Vk.Rectangle (Vk.rectPos 1 1) (Vk.rectSize 0.2 0.2)
		(Vk.rectColor 0.0 1.0 0.0 1.0) m,
	Vk.Rectangle (Vk.rectPos 1.5 (- 1.5)) (Vk.rectSize 0.3 0.6)
		(Vk.rectColor 0.0 0.0 1.0 1.0) m,
	Vk.Rectangle (Vk.rectPos (- 1.5) 1.5) (Vk.rectSize 0.6 0.3)
		(Vk.rectColor 1.0 1.0 1.0 1.0) m ]
	where m = model w h ex

model :: Float -> Float -> Vk.Extent2d -> Vk.RectModel
model w0 h0 Vk.Extent2d {
	Vk.extent2dWidth = fromIntegral -> w,
	Vk.extent2dHeight = fromIntegral -> h } = Vk.RectModel
	. Cglm.scale Cglm.mat4Identity
	$ Cglm.Vec3 $ (w0 / w) :. (h0 / h) :. 1 :. NilL
