{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Moffy
import Control.Moffy.Event.Mouse.DefaultWindow
import Control.Moffy.Viewable.Shape
import Trial.Boxes
import Trial.Paper

import Rectangles
import KeyToXKey

import Control.Monad.Fix
import Control.Monad.Trans
import Control.Concurrent
import Control.Concurrent.STM hiding (retry)
import Data.Default
import Data.List.Length
import Data.Map qualified as M
import Data.Bool
import Data.Time
import Data.KeySym
import Options.Declarative
import Gpu.Vulkan.Cglm qualified as Cglm
import Gpu.Vulkan qualified as Vk
import Graphics.UI.GlfwG.Key qualified as GlfwG.Ky
import Graphics.UI.GlfwG.Mouse qualified as GlfwG.Ms

import Control.Moffy.Event.Gui
import Control.Moffy.Event.Window
import Control.Moffy.Event.Delete
import Control.Moffy.Event.Key
import Control.Moffy.Event.Mouse (pattern OccMouseDown, pattern OccMouseUp, MouseBtn(..))
import Data.OneOrMore (project, pattern Singleton, expand)
import Data.OneOrMoreApp qualified as App (pattern Singleton, expand)

import ThreeWindows
import Control.Moffy.Handle (retry)
import Control.Moffy.Handle.TChan
import Control.Moffy.Run.TChan

main :: IO ()
main = run_ action

action :: Flag "f" '["flat"] "BOOL" "flat or not" Bool ->
	Cmd "Draw Rectangles" ()
action f = liftIO do
	((inp, (oute, outp)), ext) <- rectangles
	cow <- atomically newTChan
	cocc <- atomically newTChan
	c' <- atomically newTChan
	e <- atomically newTChan
	forkIO $ untilEnd (get f) e cow cocc ((inp, (oute, outp)), ext)
	forkIO $ interpret (retry $ handle (Just 0.5) cow cocc) c' threeWindows
	atomically $ readTChan e

data OpenWin = OpenWin

processEvReqs :: (Command Int -> STM ()) -> (STM Bool, STM (Event Int)) -> TChan (EvOccs GuiEv) -> EvReqs GuiEv -> IO ()
processEvReqs inp (oute, outp) cocc rqs = do
	case project rqs of
		Nothing -> pure ()
		Just WindowNewReq -> atomically do
			inp OpenWindow
--			writeTChan cocc . App.expand . App.Singleton . OccWindowNew $ WindowId 0
	case project rqs of
		Nothing -> pure ()
		Just (WindowDestroyReq i@(WindowId k)) -> do
			putStrLn $ "window destroy: " ++ show i
			atomically do
				inp . DestroyWindow $ fromIntegral k
				writeTChan cocc . App.expand . App.Singleton $ OccWindowDestroy i

untilEnd :: Bool -> TChan () -> TChan (EvReqs GuiEv) -> TChan (EvOccs GuiEv) ->
	((Command Int -> STM (), (STM Bool, STM (Event Int))), Int -> STM Vk.Extent2d) -> IO ()
untilEnd f e cow cocc ((inp, (oute, outp)), ext) = do
	tm0 <- getCurrentTime
	($ instances) $ fix \loop rs -> do
		threadDelay 10000
		ow <- atomically $ tryReadTChan cow
		maybe (pure ()) (processEvReqs inp (oute, outp) cocc) ow
		now <- getCurrentTime
		let	tm = realToFrac $ now `diffUTCTime` tm0
		o <- atomically do
			e0 <- ext 0
			e1 <- ext 1
			inp . Draw $ M.fromList [
				(0, ((bool (uniformBufferObject e0) def f), (rs tm))),
				(1, ((bool (uniformBufferObject e1) def f), (instances2 tm)))
				]
			bool (Just <$> outp) (pure Nothing) =<< oute
		case o of
			Nothing -> loop rs
			Just EventEnd -> putStrLn "THE WORLD ENDS" >> atomically (writeTChan e ())
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
				putStrLn "BUTTON MIDDLE DOWN"
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
			Just (EventCursorPosition _k _x _y) ->
--				putStrLn ("position: " ++ show k ++ " " ++ show (x, y)) >>
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

uniformBufferObject :: Vk.Extent2d -> ViewProjection
uniformBufferObject sce = ViewProjection {
	viewProjectionView = Cglm.lookat
		(Cglm.Vec3 $ 2 :. 2 :. 2 :. NilL)
		(Cglm.Vec3 $ 0 :. 0 :. 0 :. NilL)
		(Cglm.Vec3 $ 0 :. 0 :. 1 :. NilL),
	viewProjectionProj = Cglm.modifyMat4 1 1 negate
		$ Cglm.perspective
			(Cglm.rad 45)
			(fromIntegral (Vk.extent2dWidth sce) /
				fromIntegral (Vk.extent2dHeight sce)) 0.1 10 }

instances :: Float -> [Rectangle]
instances tm = let
	(m0, m1, m2, m3) = calcModel tm in
	[
		Rectangle (RectPos . Cglm.Vec2 $ (- 1) :. (- 1) :. NilL)
			(RectSize . Cglm.Vec2 $ 0.3 :. 0.3 :. NilL)
			(RectColor . Cglm.Vec4 $ 1.0 :. 0.0 :. 0.0 :. 1.0 :. NilL)
			m0 m1 m2 m3,
		Rectangle (RectPos . Cglm.Vec2 $ 1 :. 1 :. NilL)
			(RectSize . Cglm.Vec2 $ 0.2 :. 0.2 :. NilL)
			(RectColor . Cglm.Vec4 $ 0.0 :. 1.0 :. 0.0 :. 1.0 :. NilL)
			m0 m1 m2 m3,
		Rectangle (RectPos . Cglm.Vec2 $ 1.5 :. (- 1.5) :. NilL)
			(RectSize . Cglm.Vec2 $ 0.3 :. 0.6 :. NilL)
			(RectColor . Cglm.Vec4 $ 0.0 :. 0.0 :. 1.0 :. 1.0 :. NilL)
			m0 m1 m2 m3,
		Rectangle (RectPos . Cglm.Vec2 $ (- 1.5) :. 1.5 :. NilL)
			(RectSize . Cglm.Vec2 $ 0.6 :. 0.3 :. NilL)
			(RectColor . Cglm.Vec4 $ 1.0 :. 1.0 :. 1.0 :. 1.0 :. NilL)
			m0 m1 m2 m3
		]

instances2 :: Float -> [Rectangle]
instances2 tm = let (m0, m1, m2, m3) = calcModel tm in
	[
		Rectangle (RectPos . Cglm.Vec2 $ (- 1) :. (- 1) :. NilL)
			(RectSize . Cglm.Vec2 $ 0.3 :. 0.3 :. NilL)
			(RectColor . Cglm.Vec4 $ 0.0 :. 1.0 :. 0.0 :. 1.0 :. NilL)
			m0 m1 m2 m3,
		Rectangle (RectPos . Cglm.Vec2 $ 1 :. 1 :. NilL)
			(RectSize . Cglm.Vec2 $ 0.6 :. 0.6 :. NilL)
			(RectColor . Cglm.Vec4 $ 0.0 :. 0.0 :. 1.0 :. 1.0 :. NilL)
			m0 m1 m2 m3,
		Rectangle (RectPos . Cglm.Vec2 $ 1.5 :. (- 1.5) :. NilL)
			(RectSize . Cglm.Vec2 $ 0.6 :. 0.3 :. NilL)
			(RectColor . Cglm.Vec4 $ 1.0 :. 1.0 :. 1.0 :. 1.0 :. NilL)
			m0 m1 m2 m3,
		Rectangle (RectPos . Cglm.Vec2 $ (- 1.5) :. 1.5 :. NilL)
			(RectSize . Cglm.Vec2 $ 0.6 :. 0.3 :. NilL)
			(RectColor . Cglm.Vec4 $ 1.0 :. 0.0 :. 0.0 :. 1.0 :. NilL)
			m0 m1 m2 m3
		]

calcModel :: Float -> (RectModel0, RectModel1, RectModel2, RectModel3)
calcModel tm = let
	m0 :. m1 :. m2 :. m3 :. NilL = Cglm.mat4ToVec4s $ Cglm.rotate Cglm.mat4Identity
		(tm * Cglm.rad 90) (Cglm.Vec3 $ 0 :. 0 :. 1 :. NilL) in
	(RectModel0 m0, RectModel1 m1, RectModel2 m2, RectModel3 m3)
