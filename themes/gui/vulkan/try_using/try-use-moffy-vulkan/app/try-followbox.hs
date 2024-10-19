{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Prelude hiding (break)

import Control.Monad
import Control.Moffy
import Control.Moffy.Event.CalcTextExtents qualified as CTE

import UseCairo
import KeyToXKey

import Control.Monad.Fix
import Control.Monad.Trans
import Control.Concurrent
import Control.Concurrent.STM hiding (retry)
import Data.Default
import Data.List.Length
import Data.Map qualified as M
import Data.Bool
import Options.Declarative
import Gpu.Vulkan.Cglm qualified as Cglm
import Gpu.Vulkan qualified as Vk
import Graphics.UI.GlfwG.Mouse qualified as GlfwG.Ms

import Control.Moffy.Event.Lock
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
import Trial.Followbox.RunGtkField

import Control.Moffy.Event.Gui

import Control.Moffy.Event.Cursor
import Data.Type.Flip

import Control.Moffy.Event.CalcTextExtents (CalcTextExtents(..))

main :: IO ()
main = run_ action

action :: Flag "f" '["flat"] "BOOL" "flat or not" Bool ->
	Cmd "Draw Rectangles" ()
action f = liftIO do
	(cinp, cout) <- atomically $ (,) <$> newTChan <*> newTChan
	vext <- atomically $ newTVar M.empty
	let	inp = writeTChan cinp
		oute = isEmptyTChan cout
		outp = readTChan cout
		ext = readTVarOr (Vk.Extent2d 0 0) vext
	cow <- atomically newTChan
	cocc <- atomically newTChan
	c' <- atomically newTChan
	e <- atomically newTChan
	v <- atomically . newTVar $ View []
	_ <- forkIO $ untilEnd e cow cocc c' ((inp, (oute, outp)), ext) v
	_ <- forkIO $ runFollowboxGen cow cocc "firefox" Nothing c' do
		i <- waitFor $ adjust windowNew
		_ <- waitFor . adjust $ setCursorFromName i Default
		waitFor . adjust $ storeDefaultWindow i
		M.singleton i <$%> adjustSig (followbox i)
		waitFor . adjust $ windowDestroy i
	rectangles cinp cout vext
	atomically $ readTChan e

readTVarOr :: Ord k => a -> TVar (M.Map k (TVar a)) -> k -> STM a
readTVarOr d mp k = do
	mv <- (M.lookup k) <$> readTVar mp
	case mv of
		Nothing -> pure d
		Just v -> readTVar v

processEvReqs :: (Command Int -> STM ()) -> TChan (EvOccs GuiEv) -> EvReqs GuiEv -> IO ()
processEvReqs inp cocc rqs = do
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
	case project rqs of
		Nothing -> pure ()
		Just (CalcTextExtentsReq wid fnm fsz txt) -> do
			putStr "CalcTextExtentsReq: "
			print (wid, fnm, fsz, txt)
	case project rqs of
		Nothing -> pure ()
		Just (SetCursorFromNameReq wid nc) -> do
			putStrLn $ "SetCursorFromNameReq: " ++
				show wid ++ " " ++ show nc
			atomically do
				writeTChan cocc . App.expand . App.Singleton
					$ OccSetCursorFromName wid nc Success
	case project rqs of
		Nothing -> pure ()
		Just (SetCursorFromPngReq _ _) -> do
			putStrLn "SetCursorFromPngReq"
	case project rqs of
		Nothing -> pure ()
		Just r@(CTE.CalcTextExtentsReq _ _ _ _) -> do
			putStrLn "TextLayoutExtentReq"
			atomically . inp $ CalcTextLayoutExtent r
	case project rqs of
		Nothing -> pure ()
		Just WindowConfigureReq -> putStrLn "WindowConfigureReq"
	case project rqs of
		Nothing -> pure ()
		Just (r :: NewLockId) -> putStrLn $ "NewLockIdReq: " ++ show r

untilEnd :: TChan () -> TChan (EvReqs GuiEv) -> TChan (EvOccs GuiEv) ->
	TChan (M.Map WindowId View) ->
	((Command Int -> STM (), (STM Bool, STM (Event Int))), Int -> STM Vk.Extent2d) ->
	TVar View -> IO ()
untilEnd e cow cocc c' ((inp, (oute, outp)), ext) tvw = do
	_ <- forkIO $ forever do
		threadDelay 20000
		atomically $ inp GetEvent

	_ <- forkIO . forever $ atomically (readTChan c') >>= \vs -> do
		putStrLn $ "VIEW: " ++ show vs
		atomically . writeTVar tvw $ vs M.! WindowId 0
		e0 <- atomically $ ext 0
		atomically . inp $ Draw2 (M.fromList [(0, (def, instances' 1024 1024 e0))]) (vs M.! WindowId 0)

	_ <- forkIO $ forever do
		threadDelay 500
		ow <- atomically $ tryReadTChan cow
		maybe (pure ()) (processEvReqs inp cocc) ow

	($ instances) $ fix \loop rs -> do
		threadDelay 500
		o <- atomically do
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
			Just (EventTextLayoutExtentResult ex) -> do
				putStrLn $ "EventTextLayoutExtentResult: " ++ show ex
				atomically . writeTChan cocc
					. App.expand $ App.Singleton ex
				loop rs
			Just EventNeedRedraw -> do
				putStrLn "EVENT NEED REDRAW"
				vs <- atomically $ readTVar tvw
				e0 <- atomically $ ext 0
				atomically . inp $ Draw2 (M.fromList [(0, (def, instances' 1024 1024 e0))]) vs
				loop rs

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

instances' :: Float -> Float -> Vk.Extent2d -> [Rectangle]
instances' w h ex = let
	(m0, m1, m2, m3) = calcModel2 w h ex in
	[
--		Rectangle (RectPos . Cglm.Vec2 $ (- 1) :. (- 1) :. NilL)
		Rectangle (RectPos . Cglm.Vec2 $ (- 2) :. (- 2) :. NilL)
--			(RectSize . Cglm.Vec2 $ 0.3 :. 0.3 :. NilL)
			(RectSize . Cglm.Vec2 $ 4 :. 4 :. NilL)
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


calcModel2 :: Float -> Float -> Vk.Extent2d -> (RectModel0, RectModel1, RectModel2, RectModel3)
calcModel2 w0 h0 Vk.Extent2d { Vk.extent2dWidth = w, Vk.extent2dHeight = h } = let
	m0 :. m1 :. m2 :. m3 :. NilL = Cglm.mat4ToVec4s $ Cglm.scale Cglm.mat4Identity
		(Cglm.Vec3 $ (w0 / fromIntegral w) :. (h0 / fromIntegral h) :. 1 :. NilL) in
	(RectModel0 m0, RectModel1 m1, RectModel2 m2, RectModel3 m3)
