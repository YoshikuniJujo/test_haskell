{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Prelude hiding (break)

import Control.Monad
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
import Control.Moffy.Event.DefaultWindow
import Control.Moffy.Event.Delete hiding (deleteEvent)
import Control.Moffy.Event.Delete.DefaultWindow
import Control.Moffy.Event.Key
import Control.Moffy.Event.Mouse (
	pattern OccMouseDown, pattern OccMouseUp, pattern OccMouseMove, MouseBtn(..))
import Data.OneOrMore (project, pattern Singleton, expand)
import Data.OneOrMoreApp qualified as App (pattern Singleton, expand)

import ThreeWindows
import Control.Moffy.Handle (retry, retrySt)
import Control.Moffy.Handle.TChan
import Control.Moffy.Run.TChan

import Data.Time.Clock.System
import Trial.Boxes.RunGtkField
import Data.Type.Set
import Data.Or
import Control.Moffy.Event.Time
import Data.Maybe

main :: IO ()
main = run_ action

action :: Flag "f" '["flat"] "BOOL" "flat or not" Bool ->
	Cmd "Draw Rectangles" ()
action f = liftIO do
	(ip, op, vex) <- atomically
		$ (,,) <$> newTChan <*> newTChan <*> newTVar M.empty
	cow <- atomically newTChan
	cocc <- atomically newTChan
	c' <- atomically newTChan
	e <- atomically newTChan
	_ <- forkIO $ untilEnd (get f) e cow cocc c' (
		(writeTChan ip, (isEmptyTChan op, readTChan op)),
		(lookupOr (Vk.Extent2d 0 0) vex) )
	rectangles ip op vex
--	((inp, (oute, outp)), ext) <- rectangles
--	forkIO $ untilEnd (get f) e cow cocc c' ((inp, (oute, outp)), ext)
--	forkIO . void $ interpretSt (retrySt $ handleBoxes 0.5 cow cocc) c' threeWindows . initialBoxesState . systemToTAITime =<< getSystemTime
--	forkIO . void $ interpretSt (retrySt $ handleBoxes 0.5 cow cocc) c' bar . initialBoxesState . systemToTAITime =<< getSystemTime
	forkIO . void $ interpretSt (retrySt $ handleBoxes 0.1 cow cocc) c' baz . initialBoxesState . systemToTAITime =<< getSystemTime
{-
	forkIO $ void do
		interpretSt (retrySt $ handleBoxes 0.1 cow cocc) c' (waitFor foo) . initialBoxesState . systemToTAITime =<< getSystemTime
		putStrLn "HERE"
-}
--	forkIO . forever $ print =<< atomically (readTChanMesh 10 c')
	atomically $ readTChan e
	where
	lookupOr d t k = M.lookup k <$> readTVar t >>= maybe (pure d) readTVar

readTChanMesh :: Int -> TChan a -> STM a
readTChanMesh 0 c = readTChan c
readTChanMesh n c = readTChan c >> readTChanMesh (n - 1) c

foo :: React s (TimeEv :+: DefaultWindowEv :+: GuiEv) ()
foo = do
	i <- adjust windowNew
	adjust $ storeDefaultWindow i
	adjust $ deleteEvent `first` (drClickOn $ Rect (50, 50) (400, 400))
	adjust $ windowDestroy i

bar :: Sig s (TimeEv :+: DefaultWindowEv :+: GuiEv) BColor ()
bar = do
	wi <- waitFor do
		i <- adjust windowNew
		i <$ adjust (storeDefaultWindow i)
	adjustSig $ cycleColor `break` deleteEvent
	waitFor $ adjust $ windowDestroy wi

baz :: Sig s (TimeEv :+: DefaultWindowEv :+: GuiEv) [Box] ()
baz = do
	wi <- waitFor do
		i <- adjust windowNew
		i <$ adjust (storeDefaultWindow i)
	adjustSig $ boxes `break` deleteEvent
	waitFor $ adjust $ windowDestroy wi

data OpenWin = OpenWin

processEvReqs :: (Command Int -> STM ()) -> (STM Bool, STM (Event Int)) -> TChan (EvOccs GuiEv) -> EvReqs GuiEv -> IO ()
processEvReqs inp (oute, outp) cocc rqs = do
	case project rqs of
		Nothing -> pure ()
		Just WindowNewReq -> atomically do
			inp OpenWindow
	case project rqs of
		Nothing -> pure ()
		Just (WindowDestroyReq i@(WindowId k)) -> do
			putStrLn $ "window destroy: " ++ show i
			atomically do
				inp . DestroyWindow $ fromIntegral k
				writeTChan cocc . App.expand . App.Singleton $ OccWindowDestroy i

boxToRect :: STM Vk.Extent2d -> Box -> STM Rectangle
boxToRect ex (Box (Rect (l, u) (r, d)) clr) =
	ex >>= \(Vk.Extent2d (fromIntegral -> w) (fromIntegral -> h)) -> do
		let	l' = realToFrac $ l / w - 1
			r' = realToFrac $ r / w - 1
			u' = realToFrac $ u / h - 1
			d' = realToFrac $ d / h - 1
		pure $ Rectangle
			(RectPos . Cglm.Vec2 $ l' :. u' :. NilL)
			(RectSize . Cglm.Vec2 $ (r' - l') :. (d' - u') :. NilL)
			(RectColor . Cglm.Vec4 $ 1.0 :. 0.0 :. 0.0 :. 1.0 :. NilL)
			m
	where
	m = calcModel 0

untilEnd :: Bool -> TChan () -> TChan (EvReqs GuiEv) -> TChan (EvOccs GuiEv) -> TChan [Box] ->
	((Command Int -> STM (), (STM Bool, STM (Event Int))), Int -> STM Vk.Extent2d) -> IO ()
untilEnd f e cow cocc c' ((inp, (oute, outp)), ext) = do
	tm0 <- getCurrentTime

{-
	forkIO $ forever do
		rs' <- atomically do
--			bs <- readTChan c'
			bs <- fromMaybe [] <$> tryReadTChan c'
			boxToRect (ext 0) `mapM` bs
		atomically do
			e0 <- ext 0
--			e1 <- ext 1
			inp . Draw $ M.fromList [
--				(0, ((bool (uniformBufferObject e0) def f), (rs tm))),
				(0, ((bool (uniformBufferObject e0) def f), rs'))
--				(1, ((bool (uniformBufferObject e1) def f), (instances2 tm)))
				]
-}

	($ instances) $ fix \loop rs -> do
		threadDelay 10000
		ow <- atomically $ tryReadTChan cow
		maybe (pure ()) (processEvReqs inp (oute, outp) cocc) ow
		now <- getCurrentTime
		let	tm = realToFrac $ now `diffUTCTime` tm0
--		{-
		rs' <- atomically do
			bs <- fromMaybe [] <$> tryReadTChan c'
			boxToRect (ext 0) `mapM` bs
--			-}
		o <- atomically do
--		{-
			e0 <- ext 0
			e1 <- ext 1
			{-
			inp . Draw $ M.fromList [
--				(0, ((bool (uniformBufferObject e0) def f), (rs tm))),
				(0, ((bool (uniformBufferObject e0) def f), rs')),
				(1, ((bool (uniformBufferObject e1) def f), (instances2 tm)))
				]
				-}
--				-}
			bool (Just <$> outp) (pure Nothing) =<< oute
		putStrLn "HERE"
		case o of
			Nothing -> putStrLn "NO EVENT" >> loop rs
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
--				putStrLn "BUTTON MIDDLE DOWN"
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
--				putStrLn ("position: " ++ show k ++ " " ++ show (x, y))
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
	m = calcModel tm in
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
