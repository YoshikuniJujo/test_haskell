{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Prelude hiding (break)

import Control.Monad
import Control.Moffy
import Control.Moffy.Viewable.Shape
import Trial.Boxes

import VulkanRectangles qualified as Vk
import KeyToXKey

import Control.Monad.Fix
import Control.Concurrent
import Control.Concurrent.STM hiding (retry)
import Data.List.Length
import Data.Map qualified as M
import Gpu.Vulkan.Cglm qualified as Cglm
import Gpu.Vulkan qualified as Vk
import Graphics.UI.GlfwG.Key qualified as GlfwG.Ky
import Graphics.UI.GlfwG.Mouse qualified as GlfwG.Ms

import Control.Moffy.Event.Gui
import Control.Moffy.Event.Window
import Control.Moffy.Event.DefaultWindow
import Control.Moffy.Event.Delete hiding (deleteEvent)
import Control.Moffy.Event.Delete.DefaultWindow
import Control.Moffy.Event.Key
import Control.Moffy.Event.Mouse (
	pattern OccMouseDown, pattern OccMouseUp, pattern OccMouseMove, MouseBtn(..))
import Data.OneOrMore (project)
import Data.OneOrMoreApp qualified as App (pattern Singleton, expand)

import Control.Moffy.Handle (retrySt)
import Control.Moffy.Run.TChan

import Data.Time.Clock.System
import Trial.Boxes.RunGtkField (handleBoxes, initialBoxesState)
import Data.Type.Set
import Control.Moffy.Event.Time

----------------------------------------------------------------------
--
-- * MAIN
-- * MOFFY EVENT REQUEST TO VULKAN COMMAND
-- * EVENT FROM VULKAN TO MOFFY
-- * BOX TO RECTANGLE
-- * RUN BOXES
-- * RECTANGLES
--
----------------------------------------------------------------------

-- MAIN

main :: IO ()
main = do
	(ccmd@(writeTChan -> cmd), cev@(readTChan -> ev), ex) <- atomically
		$ (,,) <$> newTChan <*> newTChan <*> newTVar M.empty
	ff $ threadDelay 20000 >> atomically (cmd Vk.GetEvent)
	(crqs@(readTChan -> rqs), cocc@(writeTChan -> occ))
		<- atomically $ (,) <$> newTChan <*> newTChan
	ffa $ mffReqsToVk cmd occ =<< rqs; f $ vkEvToMoffy ev occ
	cbxs@(readTChan -> bxs) <- atomically newTChan
	ffa $ cmd . Vk.Draw . M.singleton 0 =<< (boxToRect ex `mapM`) =<< bxs
	f $ interpretSt (retrySt $ handleBoxes 0.1 crqs cocc) cbxs
		runBoxes . initialBoxesState . systemToTAITime =<< getSystemTime
	Vk.rectangles ccmd cev ex
	where
	f = void . forkIO . void
	ff = void . forkIO . forever; ffa = ff . atomically
	
-- MOFFY EVENT REQUEST TO VULKAN COMMAND

mffReqsToVk :: (Vk.Command Int -> STM ()) ->
	(EvOccs GuiEv -> STM ()) -> EvReqs GuiEv -> STM ()
mffReqsToVk cmd occ rqs = do
	maybeWhen (project rqs) \WindowNewReq -> cmd Vk.OpenWindow
	maybeWhen (project rqs)
		\(WindowDestroyReq i@(WindowId k)) -> do
		cmd . Vk.DestroyWindow $ fromIntegral k
		occ . App.expand . App.Singleton $ OccWindowDestroy i
	where maybeWhen = maybe (const $ pure ()) (flip ($))

-- EVENT FROM VULKAN TO MOFFY

vkEvToMoffy :: STM (Vk.Event Int) -> (EvOccs GuiEv -> STM ())  -> IO ()
vkEvToMoffy ev occ = ($ rects1) $ fix \go rs -> atomically ev >>= \case
	Vk.EventEnd -> pure ()
	Vk.EventOpenWindow k ->
		atomically (occ . App.expand . App.Singleton
			. OccWindowNew . WindowId $ fromIntegral k) >> go rs
	Vk.EventDeleteWindow k -> do
		putStrLn $ "delete window: " ++ show k
		atomically . occ
			. App.expand . App.Singleton
			. OccDeleteEvent . WindowId $ fromIntegral k
		go rs
	Vk.EventKeyDown k GlfwG.Ky.Key'D -> do
		putStrLn $ "delete window by key `d': " ++ show k
		atomically . occ
			. App.expand . App.Singleton
			. OccDeleteEvent . WindowId $ fromIntegral k
		go rs
	Vk.EventKeyDown w ky -> do
		putStrLn $ "KEY DOWN: " ++ show w ++ " " ++ show ky
		atomically . occ
			. App.expand . App.Singleton
			. OccKeyDown (WindowId $ fromIntegral w) $ keyToXKey ky
		go rs
	Vk.EventKeyUp w ky -> do
		putStrLn $ "KEY UP  : " ++ show w ++ " " ++ show ky
		go rs
	Vk.EventMouseButtonDown w (buttonToButton -> mb) -> do
		atomically . occ . App.expand . App.Singleton
			$ OccMouseDown (WindowId $ fromIntegral w) mb
		go rects1
	Vk.EventMouseButtonUp w (buttonToButton -> mb) -> do
		atomically . occ . App.expand . App.Singleton
			$ OccMouseUp (WindowId $ fromIntegral w) mb
		go rs
	Vk.EventCursorPosition k x y -> do
		atomically . occ
			. App.expand . App.Singleton
			$ OccMouseMove (WindowId $ fromIntegral k) (x, y)
		go rs

buttonToButton :: GlfwG.Ms.MouseButton -> MouseBtn
buttonToButton = \case
	GlfwG.Ms.MouseButton'1 -> ButtonLeft
	GlfwG.Ms.MouseButton'2 -> ButtonRight
	GlfwG.Ms.MouseButton'3 -> ButtonMiddle
	_ -> ButtonUnknown maxBound

-- BOX TO RECTANGLE

boxToRect :: (Ord k, Num k) =>
	TVar (M.Map k (TVar Vk.Extent2d)) -> Box -> STM Vk.Rectangle
boxToRect ex (Box (Rect (l, u) (r, d)) clr) =
	lookupOr (Vk.Extent2d 0 0) ex 0 >>= \(Vk.Extent2d (fromIntegral -> w) (fromIntegral -> h)) -> do
		let	l' = realToFrac $ 4 * l / w - 2
			r' = realToFrac $ 4 * r / w - 2
			u' = realToFrac $ 4 * u / h - 2
			d' = realToFrac $ 4 * d / h - 2
		pure $ Vk.Rectangle
			(Vk.RectPos . Cglm.Vec2 $ l' :. u' :. NilL)
			(Vk.RectSize . Cglm.Vec2 $ (r' - l') :. (d' - u') :. NilL)
			(colorToColor clr)
			m
	where m = calcModel 0

colorToColor :: BColor -> Vk.RectColor
colorToColor = \case
	Red -> Vk.RectColor . Cglm.Vec4 $ 1.0 :. 0.0 :. 0.0 :. 1.0 :. NilL
	Green -> Vk.RectColor . Cglm.Vec4 $ 0.0 :. 1.0 :. 0.0 :. 1.0 :. NilL
	Blue -> Vk.RectColor . Cglm.Vec4 $ 0.0 :. 0.0 :. 1.0 :. 1.0 :. NilL
	Yellow -> Vk.RectColor . Cglm.Vec4 $ 1.0 :. 1.0 :. 0.0 :. 1.0 :. NilL
	Cyan -> Vk.RectColor . Cglm.Vec4 $ 0.0 :. 1.0 :. 1.0 :. 1.0 :. NilL
	Magenta -> Vk.RectColor . Cglm.Vec4 $ 1.0 :. 0.0 :. 1.0 :. 1.0 :. NilL

lookupOr :: Ord k => b -> TVar (M.Map k (TVar b)) -> k -> STM b
lookupOr d t k = M.lookup k <$> readTVar t >>= maybe (pure d) readTVar

-- RUN BOXES

runBoxes :: Sig s (TimeEv :+: DefaultWindowEv :+: GuiEv) [Box] ()
runBoxes = do
	wi <- waitFor $ adjust windowNew >>= \i ->
		i <$ adjust (storeDefaultWindow i)
	_ <- adjustSig $ boxes `break` deleteEvent
	waitFor $ adjust $ windowDestroy wi

-- RECTANGLES

rects1 :: Float -> [Vk.Rectangle]
rects1 tm = let m = calcModel tm in
	[
		Vk.Rectangle (Vk.RectPos . Cglm.Vec2 $ (- 1) :. (- 1) :. NilL)
			(Vk.RectSize . Cglm.Vec2 $ 0.3 :. 0.3 :. NilL)
			(Vk.RectColor . Cglm.Vec4 $ 1.0 :. 0.0 :. 0.0 :. 1.0 :. NilL)
			m,
		Vk.Rectangle (Vk.RectPos . Cglm.Vec2 $ 1 :. 1 :. NilL)
			(Vk.RectSize . Cglm.Vec2 $ 0.2 :. 0.2 :. NilL)
			(Vk.RectColor . Cglm.Vec4 $ 0.0 :. 1.0 :. 0.0 :. 1.0 :. NilL)
			m,
		Vk.Rectangle (Vk.RectPos . Cglm.Vec2 $ 1.5 :. (- 1.5) :. NilL)
			(Vk.RectSize . Cglm.Vec2 $ 0.3 :. 0.6 :. NilL)
			(Vk.RectColor . Cglm.Vec4 $ 0.0 :. 0.0 :. 1.0 :. 1.0 :. NilL)
			m,
		Vk.Rectangle (Vk.RectPos . Cglm.Vec2 $ (- 1.5) :. 1.5 :. NilL)
			(Vk.RectSize . Cglm.Vec2 $ 0.6 :. 0.3 :. NilL)
			(Vk.RectColor . Cglm.Vec4 $ 1.0 :. 1.0 :. 1.0 :. 1.0 :. NilL)
			m
		]

calcModel :: Float -> Vk.RectModel
calcModel tm = Vk.RectModel $ Cglm.rotate Cglm.mat4Identity
		(tm * Cglm.rad 90) (Cglm.Vec3 $ 0 :. 0 :. 1 :. NilL)
