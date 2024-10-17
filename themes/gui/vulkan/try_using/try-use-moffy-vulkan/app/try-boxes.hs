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

import Rectangles2
import KeyToXKey

import Control.Monad.Fix
import Control.Concurrent
import Control.Concurrent.STM hiding (retry)
import Data.List.Length
import Data.Map qualified as M
import Data.Bool
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
import Trial.Boxes.RunGtkField
import Data.Type.Set
import Control.Moffy.Event.Time

----------------------------------------------------------------------
--
-- * MAIN
-- * BODY
-- * RECTANGLES
--
----------------------------------------------------------------------

-- MAIN

main :: IO ()
main = do
	(ccmd, cev, vex) <- atomically
		$ (,,) <$> newTChan <*> newTChan <*> newTVar M.empty
	let	inp = writeTChan ccmd
		oute = isEmptyTChan cev
		outp = readTChan cev
		ext = lookupOr (Vk.Extent2d 0 0) vex
	cow <- atomically newTChan
	cocc <- atomically newTChan
	c' <- atomically newTChan
	_ <- forkIO . forever
		$ threadDelay 20000 >> atomically (writeTChan ccmd GetEvent)
	_ <- forkIO $ body cow cocc c' ((inp, (oute, outp)), ext)
	_ <- forkIO . void $ interpretSt (retrySt $ handleBoxes 0.1 cow cocc) c' baz . initialBoxesState . systemToTAITime =<< getSystemTime
	rectangles2 ccmd cev vex
	where
	lookupOr d t k = M.lookup k <$> readTVar t >>= maybe (pure d) readTVar

baz :: Sig s (TimeEv :+: DefaultWindowEv :+: GuiEv) [Box] ()
baz = do
	wi <- waitFor do
		i <- adjust windowNew
		i <$ adjust (storeDefaultWindow i)
	_ <- adjustSig $ boxes `break` deleteEvent
	waitFor $ adjust $ windowDestroy wi

-- BODY

body :: TChan (EvReqs GuiEv) -> TChan (EvOccs GuiEv) -> TChan [Box] ->
	((Command Int -> STM (), (STM Bool, STM (Event Int))), Int -> STM Vk.Extent2d) -> IO ()
body cow cocc c' ((cmd, (oute, outp)), ext) = do
	_ <- forkIO $ forever do
		rs' <- atomically do
			bs <- readTChan c'
			boxToRect (ext 0) `mapM` bs
		atomically do
			cmd . Draw $ M.fromList [(0, rs')]

	_ <- forkIO $ forever do
		threadDelay 500
		ow <- atomically $ tryReadTChan cow
		maybe (pure ()) (processEvReqs cmd cocc) ow

	($ instances) $ fix \loop rs -> do
		threadDelay 500
		o <- atomically do
			bool (Just <$> outp) (pure Nothing) =<< oute
		case o of
			Nothing -> loop rs
			Just EventEnd -> pure ()
			Just (EventKeyDown k GlfwG.Ky.Key'D) -> do
				putStrLn $ "delete window by key `d': " ++ show k
				atomically . writeTChan cocc
					. App.expand . App.Singleton
					. OccDeleteEvent . WindowId $ fromIntegral k
				loop rs
			Just (EventKeyDown w ky) -> do
				putStrLn $ "KEY DOWN: " ++ show w ++ " " ++ show ky
				atomically . writeTChan cocc
					. App.expand . App.Singleton
					. OccKeyDown (WindowId $ fromIntegral w) $ keyToXKey ky
				loop rs
			Just (EventKeyUp w ky) -> do
				putStrLn $ "KEY UP  : " ++ show w ++ " " ++ show ky
				loop rs
			Just (EventMouseButtonDown w GlfwG.Ms.MouseButton'1) -> do
				putStrLn "BUTTON LEFT DOWN"
				atomically . writeTChan cocc
					. App.expand . App.Singleton
					$ OccMouseDown (WindowId $ fromIntegral w) ButtonLeft
				loop instances
			Just (EventMouseButtonDown w GlfwG.Ms.MouseButton'2) -> do
				putStrLn "BUTTON RIGHT DOWN"
				atomically . writeTChan cocc
					. App.expand . App.Singleton
					$ OccMouseDown (WindowId $ fromIntegral w) ButtonRight
				loop instances2
			Just (EventMouseButtonDown w GlfwG.Ms.MouseButton'3) -> do
				atomically . writeTChan cocc
					. App.expand . App.Singleton
					$ OccMouseDown (WindowId $ fromIntegral w) ButtonMiddle
				loop rs
			Just (EventMouseButtonUp w GlfwG.Ms.MouseButton'1) -> do
				atomically . writeTChan cocc
					. App.expand . App.Singleton
					$ OccMouseUp (WindowId $ fromIntegral w) ButtonLeft
				loop rs
			Just (EventMouseButtonUp w GlfwG.Ms.MouseButton'2) -> do
				atomically . writeTChan cocc
					. App.expand . App.Singleton
					$ OccMouseUp (WindowId $ fromIntegral w) ButtonRight
				loop rs
			Just (EventMouseButtonUp w GlfwG.Ms.MouseButton'3) -> do
				atomically . writeTChan cocc
					. App.expand . App.Singleton
					$ OccMouseUp (WindowId $ fromIntegral w) ButtonMiddle
				loop rs
			Just (EventMouseButtonDown _ _) -> loop rs
			Just (EventMouseButtonUp _ _) -> loop rs
			Just (EventCursorPosition k x y) -> do
				atomically . writeTChan cocc
					. App.expand . App.Singleton
					$ OccMouseMove (WindowId $ fromIntegral k) (x, y)
				loop rs
			Just (EventOpenWindow k) -> do
				putStrLn $ "open window: " ++ show k
				atomically . writeTChan cocc
					. App.expand . App.Singleton
					. OccWindowNew . WindowId $ fromIntegral k
				loop rs
			Just (EventDeleteWindow k) -> do
				putStrLn $ "delete window: " ++ show k
				atomically . writeTChan cocc
					. App.expand . App.Singleton
					. OccDeleteEvent . WindowId $ fromIntegral k
				loop rs

processEvReqs :: (Command Int -> STM ()) -> TChan (EvOccs GuiEv) -> EvReqs GuiEv -> IO ()
processEvReqs cmd cocc rqs = do
	case project rqs of
		Nothing -> pure ()
		Just WindowNewReq -> atomically do
			cmd OpenWindow
	case project rqs of
		Nothing -> pure ()
		Just (WindowDestroyReq i@(WindowId k)) -> do
			putStrLn $ "window destroy: " ++ show i
			atomically do
				cmd . DestroyWindow $ fromIntegral k
				writeTChan cocc . App.expand . App.Singleton $ OccWindowDestroy i

boxToRect :: STM Vk.Extent2d -> Box -> STM Rectangle
boxToRect ex (Box (Rect (l, u) (r, d)) clr) =
	ex >>= \(Vk.Extent2d (fromIntegral -> w) (fromIntegral -> h)) -> do
		let	l' = realToFrac $ 4 * l / w - 2
			r' = realToFrac $ 4 * r / w - 2
			u' = realToFrac $ 4 * u / h - 2
			d' = realToFrac $ 4 * d / h - 2
		pure $ Rectangle
			(RectPos . Cglm.Vec2 $ l' :. u' :. NilL)
			(RectSize . Cglm.Vec2 $ (r' - l') :. (d' - u') :. NilL)
			(colorToColor clr)
			m
	where m = calcModel 0

colorToColor :: BColor -> RectColor
colorToColor = \case
	Red -> RectColor . Cglm.Vec4 $ 1.0 :. 0.0 :. 0.0 :. 1.0 :. NilL
	Green -> RectColor . Cglm.Vec4 $ 0.0 :. 1.0 :. 0.0 :. 1.0 :. NilL
	Blue -> RectColor . Cglm.Vec4 $ 0.0 :. 0.0 :. 1.0 :. 1.0 :. NilL
	Yellow -> RectColor . Cglm.Vec4 $ 1.0 :. 1.0 :. 0.0 :. 1.0 :. NilL
	Cyan -> RectColor . Cglm.Vec4 $ 0.0 :. 1.0 :. 1.0 :. 1.0 :. NilL
	Magenta -> RectColor . Cglm.Vec4 $ 1.0 :. 0.0 :. 1.0 :. 1.0 :. NilL

-- RECTANGLES

instances :: Float -> [Rectangle]
instances tm = let m = calcModel tm in
	[
		Rectangle (RectPos . Cglm.Vec2 $ (- 1) :. (- 1) :. NilL)
			(RectSize . Cglm.Vec2 $ 0.3 :. 0.3 :. NilL)
			(RectColor . Cglm.Vec4 $ 1.0 :. 0.0 :. 0.0 :. 1.0 :. NilL)
			m,
		Rectangle (RectPos . Cglm.Vec2 $ 1 :. 1 :. NilL)
			(RectSize . Cglm.Vec2 $ 0.2 :. 0.2 :. NilL)
			(RectColor . Cglm.Vec4 $ 0.0 :. 1.0 :. 0.0 :. 1.0 :. NilL)
			m,
		Rectangle (RectPos . Cglm.Vec2 $ 1.5 :. (- 1.5) :. NilL)
			(RectSize . Cglm.Vec2 $ 0.3 :. 0.6 :. NilL)
			(RectColor . Cglm.Vec4 $ 0.0 :. 0.0 :. 1.0 :. 1.0 :. NilL)
			m,
		Rectangle (RectPos . Cglm.Vec2 $ (- 1.5) :. 1.5 :. NilL)
			(RectSize . Cglm.Vec2 $ 0.6 :. 0.3 :. NilL)
			(RectColor . Cglm.Vec4 $ 1.0 :. 1.0 :. 1.0 :. 1.0 :. NilL)
			m
		]

instances2 :: Float -> [Rectangle]
instances2 tm = let m = calcModel tm in
	[
		Rectangle (RectPos . Cglm.Vec2 $ (- 1) :. (- 1) :. NilL)
			(RectSize . Cglm.Vec2 $ 0.3 :. 0.3 :. NilL)
			(RectColor . Cglm.Vec4 $ 0.0 :. 1.0 :. 0.0 :. 1.0 :. NilL)
			m,
		Rectangle (RectPos . Cglm.Vec2 $ 1 :. 1 :. NilL)
			(RectSize . Cglm.Vec2 $ 0.6 :. 0.6 :. NilL)
			(RectColor . Cglm.Vec4 $ 0.0 :. 0.0 :. 1.0 :. 1.0 :. NilL)
			m,
		Rectangle (RectPos . Cglm.Vec2 $ 1.5 :. (- 1.5) :. NilL)
			(RectSize . Cglm.Vec2 $ 0.6 :. 0.3 :. NilL)
			(RectColor . Cglm.Vec4 $ 1.0 :. 1.0 :. 1.0 :. 1.0 :. NilL)
			m,
		Rectangle (RectPos . Cglm.Vec2 $ (- 1.5) :. 1.5 :. NilL)
			(RectSize . Cglm.Vec2 $ 0.6 :. 0.3 :. NilL)
			(RectColor . Cglm.Vec4 $ 1.0 :. 0.0 :. 0.0 :. 1.0 :. NilL)
			m
		]

calcModel :: Float -> RectModel
calcModel tm = RectModel $ Cglm.rotate Cglm.mat4Identity
		(tm * Cglm.rad 90) (Cglm.Vec3 $ 0 :. 0 :. 1 :. NilL)
