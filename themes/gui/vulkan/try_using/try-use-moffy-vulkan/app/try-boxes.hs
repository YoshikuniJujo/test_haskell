{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Prelude hiding (break)

import Control.Monad
import Control.Monad.Fix
import Control.Concurrent
import Control.Concurrent.STM hiding (retry)

import Control.Moffy
import Control.Moffy.Event.Time
import Control.Moffy.Event.Gui
import Control.Moffy.Event.Window
import Control.Moffy.Event.DefaultWindow
import Control.Moffy.Event.Delete hiding (deleteEvent)
import Control.Moffy.Event.Delete.DefaultWindow
import Control.Moffy.Event.Key
import Control.Moffy.Event.Mouse (
	MouseBtn(..),
	pattern OccMouseDown, pattern OccMouseUp, pattern OccMouseMove )
import Control.Moffy.Handle (retrySt, ExpandableOccurred)
import Control.Moffy.Run.TChan
import Control.Moffy.Viewable.Shape

import Data.Type.Set
import Data.OneOrMore (project)
import Data.OneOrMoreApp qualified as App (pattern Singleton, expand)
import Data.Map qualified as M
import Data.Time.Clock.System
import Graphics.UI.GlfwG.Key qualified as GlfwG.Ky
import Graphics.UI.GlfwG.Mouse qualified as GlfwG.Ms
import Gpu.Vulkan qualified as Vk
import Gpu.Vulkan.Cglm qualified as Cglm

import Trial.Boxes
import Trial.Boxes.RunGtkField (handleBoxes, initialBoxesState)
import VulkanRectangles qualified as Vk
import KeyToXKey

----------------------------------------------------------------------
--
-- * MAIN
-- * SEND REQUEST/EVENT TO VULKAN/MOFFY
-- * BOX TO RECTANGLE
-- * RUN BOXES
--
----------------------------------------------------------------------

-- MAIN

main :: IO ()
main = do
	(ccmd@(writeTChan -> cmd), cev@(readTChan -> ev), cex) <- atomically
		$ (,,) <$> newTChan <*> newTChan <*> newTVar M.empty
	ff $ threadDelay 20000 >> atomically (cmd Vk.GetEvent)
	(crqs@(readTChan -> rqs), cocc@(writeTChan -> occ))
		<- atomically $ (,) <$> newTChan <*> newTChan
	ffa $ mffReqsToVk cmd occ =<< rqs; f $ vkEvToMoffy ev occ
	cbxs@(readTChan -> bxs) <- atomically newTChan
	ffa $ cmd . Vk.Draw . M.singleton 0 =<< (boxToRect cex `mapM`) =<< bxs
	f $ interpretSt (retrySt $ handleBoxes 0.1 crqs cocc) cbxs
		runBoxes . initialBoxesState . systemToTAITime =<< getSystemTime
	Vk.rectangles ccmd cev cex
	where
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
	where maybeWhen = maybe (const $ pure ()) (flip ($))

vkEvToMoffy :: STM (Vk.Event Int) -> (EvOccs GuiEv -> STM ())  -> IO ()
vkEvToMoffy ev occ = fix \go -> atomically ev >>= \case
	Vk.EventEnd -> pure ()
	Vk.EventOpenWindow (w -> i) -> o (OccWindowNew i) >> go
	Vk.EventDeleteWindow (w -> i) -> o (OccDeleteEvent i) >> go
	Vk.EventKeyDown (w -> i) GlfwG.Ky.Key'D -> o (OccDeleteEvent i) >> go
	Vk.EventKeyDown (w -> i) ky -> o (OccKeyDown i $ keyToXKey ky) >> go
	Vk.EventKeyUp (w -> i) ky -> o (OccKeyUp i $ keyToXKey ky) >> go
	Vk.EventMouseButtonDown (w -> i) (bt -> b) -> o (OccMouseDown i b) >> go
	Vk.EventMouseButtonUp (w -> i) (bt -> b) -> o (OccMouseUp i b) >> go
	Vk.EventCursorPosition (w -> i) x y -> o (OccMouseMove i (x, y)) >> go
	where
	o :: ExpandableOccurred (Singleton a) GuiEv => Occurred a -> IO ()
	o = atomically . occ . App.expand . App.Singleton
	w = WindowId . fromIntegral
	bt = \case
		GlfwG.Ms.MouseButton'1 -> ButtonLeft
		GlfwG.Ms.MouseButton'2 -> ButtonRight
		GlfwG.Ms.MouseButton'3 -> ButtonMiddle
		_ -> ButtonUnknown maxBound

-- BOX TO RECTANGLE

boxToRect :: (Ord k, Num k) =>
	TVar (M.Map k (TVar Vk.Extent2d)) -> Box -> STM Vk.Rectangle
boxToRect ex (Box (Rect (l, t) (r, b)) clr) = (<$> sz) \(w, h) ->
	let	(l', r', t', b') = (cnv l w, cnv r w, cnv t h, cnv b h) in
	Vk.Rectangle
		(Vk.rectPos l' t') (Vk.rectSize (r' - l') (b' - t'))
		(colorToColor clr) (Vk.RectModel Cglm.mat4Identity)
	where
	sz = (<$> lookupOr (Vk.Extent2d 0 0) ex 0)
		\(Vk.Extent2d (fromIntegral -> w) (fromIntegral -> h)) -> (w, h)
	cnv xy wh = realToFrac $ 4 * xy / wh - 2

colorToColor :: BColor -> Vk.RectColor
colorToColor = ($ 1.0) . \case
	Red -> Vk.rectColor 1.0 0.0 0.0; Green -> Vk.rectColor 0.0 1.0 0.0
	Blue -> Vk.rectColor 0.0 0.0 1.0; Yellow -> Vk.rectColor 1.0 1.0 0.0
	Cyan -> Vk.rectColor 0.0 1.0 1.0; Magenta -> Vk.rectColor 1.0 0.0 1.0

lookupOr :: Ord k => a -> TVar (M.Map k (TVar a)) -> k -> STM a
lookupOr d t k = M.lookup k <$> readTVar t >>= maybe (pure d) readTVar

-- RUN BOXES

runBoxes :: Sig s (TimeEv :+: DefaultWindowEv :+: GuiEv) [Box] ()
runBoxes = do
	wi <- waitFor
		$ adjust windowNew >>= \i -> i <$ adjust (storeDefaultWindow i)
	_ <- adjustSig $ boxes `break` deleteEvent
	waitFor . adjust $ windowDestroy wi
