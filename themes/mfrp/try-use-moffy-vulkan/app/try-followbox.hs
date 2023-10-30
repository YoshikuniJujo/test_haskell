{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Prelude hiding (break)

import Control.Monad
import Control.Moffy
import Control.Moffy.Event.Mouse.DefaultWindow
import Control.Moffy.Event.CalcTextExtents qualified as CTE
import Control.Moffy.Viewable.Shape
import Trial.Boxes
import Trial.Paper

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
import Data.Time
import Data.KeySym
import Options.Declarative
import Gpu.Vulkan.Cglm qualified as Cglm
import Gpu.Vulkan qualified as Vk
import Graphics.UI.GlfwG.Key qualified as GlfwG.Ky
import Graphics.UI.GlfwG.Mouse qualified as GlfwG.Ms

-- import Control.Moffy.Event.Gui
import Control.Moffy.Event.Lock.Internal
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

import Trial.Followbox
import Trial.Followbox.ViewType
import Trial.Followbox.RunGtkField

import Control.Moffy.Event.Gui

import Control.Moffy.Event.Cursor
import Control.Moffy.Event.DefaultWindow
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
	x <- atomically newTChan
	forkIO $ untilEnd (get f) e cow cocc c' ((inp, (oute, outp)), ext)
	forkIO $ runFollowboxGen cow cocc "firefox" Nothing c' do
		i <- waitFor $ adjust windowNew
		_ <- waitFor . adjust $ setCursorFromName i Default
		waitFor . adjust $ storeDefaultWindow i
		M.singleton i <$%> adjustSig (followbox i)
		waitFor . adjust $ windowDestroy i
--	forkIO . void $ interpretSt (retrySt $ handleBoxes 0.1 cow cocc) c' baz . initialBoxesState . systemToTAITime =<< getSystemTime
--	forkIO . forever $ putStrLn . ("VIEW: " ++) . show =<< atomically (readTChan c')
	rectangles2 cinp cout vext
	atomically $ readTChan e

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
		Just (SetCursorFromPngReq wid nc) -> do
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
			m0 m1 m2 m3
	where
	(m0, m1, m2, m3) = calcModel 0

colorToColor :: BColor -> RectColor
colorToColor = \case
	Red -> RectColor . Cglm.Vec4 $ 1.0 :. 0.0 :. 0.0 :. 1.0 :. NilL
	Green -> RectColor . Cglm.Vec4 $ 0.0 :. 1.0 :. 0.0 :. 1.0 :. NilL
	Blue -> RectColor . Cglm.Vec4 $ 0.0 :. 0.0 :. 1.0 :. 1.0 :. NilL
	Yellow -> RectColor . Cglm.Vec4 $ 1.0 :. 1.0 :. 0.0 :. 1.0 :. NilL
	Cyan -> RectColor . Cglm.Vec4 $ 0.0 :. 1.0 :. 1.0 :. 1.0 :. NilL
	Magenta -> RectColor . Cglm.Vec4 $ 1.0 :. 0.0 :. 1.0 :. 1.0 :. NilL

untilEnd :: Bool -> TChan () -> TChan (EvReqs GuiEv) -> TChan (EvOccs GuiEv) ->
	TChan (M.Map WindowId View) ->
	((Command Int -> STM (), (STM Bool, STM (Event Int))), Int -> STM Vk.Extent2d) -> IO ()
untilEnd f e cow cocc c' ((inp, (oute, outp)), ext) = do
	tm0 <- getCurrentTime

	forkIO $ forever do
		threadDelay 20000
		atomically $ inp GetEvent

	forkIO . forever $ atomically (readTChan c') >>= \vs -> do
		putStrLn $ "VIEW: " ++ show vs
		atomically
			$ inp . Draw $ M.fromList [(0, (def, instances 0))]

{-
	forkIO $ forever do
		rs' <- atomically do
			bs <- readTChan c'
			boxToRect (ext 0) `mapM` bs
		atomically do
			inp . Draw $ M.fromList [(0, (def, rs'))]
			-}

	forkIO $ forever do
		threadDelay 500
		ow <- atomically $ tryReadTChan cow
		maybe (pure ()) (processEvReqs inp (oute, outp) cocc) ow

	($ instances) $ fix \loop rs -> do
		threadDelay 500
		now <- getCurrentTime
		let	tm = realToFrac $ now `diffUTCTime` tm0
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
