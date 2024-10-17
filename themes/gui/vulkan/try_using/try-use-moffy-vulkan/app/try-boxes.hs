{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Prelude hiding (break)

import Control.Arrow ((&&&))
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
-- * EVENT REQUEST
-- * BOX TO RECTANGLE
-- * RUN BOXES
-- * EVENT FROM VULKAN TO MOFFY
-- * RECTANGLES
--
----------------------------------------------------------------------

-- MAIN

main :: IO ()
main = do
	(cmd, ev, ex) <- atomically
		$ (,,) <$> newTChan <*> newTChan <*> newTVar M.empty
	ff $ threadDelay 20000 >> atomically (writeTChan cmd Vk.GetEvent)
	(crqs, cocc) <- atomically $ (,) <$> newTChan <*> newTChan
	ff $ mffReqsToVk (writeTChan cmd) cocc =<< atomically (readTChan crqs)
	f $ vkEvToMoffy (isEmptyTChan &&& readTChan $ ev) cocc
	bxs <- atomically newTChan
	ffa $ writeTChan cmd . Vk.Draw
		. M.singleton 0 =<< (boxToRect ex `mapM`) =<< readTChan bxs
	f $ interpretSt (retrySt $ handleBoxes 0.1 crqs cocc) bxs
		baz . initialBoxesState . systemToTAITime =<< getSystemTime
	Vk.rectangles cmd ev ex
	where
	f = void . forkIO . void
	ff = void . forkIO . forever; ffa = ff . atomically
	
-- MOFFY EVENT REQUEST TO VULKAN COMMAND

mffReqsToVk ::
	(Vk.Command Int -> STM ()) -> TChan (EvOccs GuiEv) -> EvReqs GuiEv -> IO ()
mffReqsToVk cmd cocc rqs = do
	maybeWhen (project rqs) \WindowNewReq -> atomically $ cmd Vk.OpenWindow
	maybeWhen (project rqs)
		\(WindowDestroyReq i@(WindowId k)) -> atomically do
		cmd . Vk.DestroyWindow $ fromIntegral k
		writeTChan cocc . App.expand . App.Singleton $ OccWindowDestroy i

maybeWhen :: Monad m => Maybe a -> (a -> m ()) -> m ()
maybeWhen = maybe (const $ pure ()) (flip ($))

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

baz :: Sig s (TimeEv :+: DefaultWindowEv :+: GuiEv) [Box] ()
baz = do
	wi <- waitFor do
		i <- adjust windowNew
		i <$ adjust (storeDefaultWindow i)
	_ <- adjustSig $ boxes `break` deleteEvent
	waitFor $ adjust $ windowDestroy wi

-- EVENT FROM VULKAN TO MOFFY

vkEvToMoffy :: (STM Bool, STM (Vk.Event Int)) -> TChan (EvOccs GuiEv) -> IO ()
vkEvToMoffy (nev, ev) cocc = do

	($ instances) $ fix \loop rs -> do
		o <- atomically do
			bool (Just <$> ev) (pure Nothing) =<< nev
		case o of
			Nothing -> loop rs
			Just Vk.EventEnd -> pure ()
			Just (Vk.EventKeyDown k GlfwG.Ky.Key'D) -> do
				putStrLn $ "delete window by key `d': " ++ show k
				atomically . writeTChan cocc
					. App.expand . App.Singleton
					. OccDeleteEvent . WindowId $ fromIntegral k
				loop rs
			Just (Vk.EventKeyDown w ky) -> do
				putStrLn $ "KEY DOWN: " ++ show w ++ " " ++ show ky
				atomically . writeTChan cocc
					. App.expand . App.Singleton
					. OccKeyDown (WindowId $ fromIntegral w) $ keyToXKey ky
				loop rs
			Just (Vk.EventKeyUp w ky) -> do
				putStrLn $ "KEY UP  : " ++ show w ++ " " ++ show ky
				loop rs
			Just (Vk.EventMouseButtonDown w GlfwG.Ms.MouseButton'1) -> do
				putStrLn "BUTTON LEFT DOWN"
				atomically . writeTChan cocc
					. App.expand . App.Singleton
					$ OccMouseDown (WindowId $ fromIntegral w) ButtonLeft
				loop instances
			Just (Vk.EventMouseButtonDown w GlfwG.Ms.MouseButton'2) -> do
				putStrLn "BUTTON RIGHT DOWN"
				atomically . writeTChan cocc
					. App.expand . App.Singleton
					$ OccMouseDown (WindowId $ fromIntegral w) ButtonRight
				loop instances2
			Just (Vk.EventMouseButtonDown w GlfwG.Ms.MouseButton'3) -> do
				atomically . writeTChan cocc
					. App.expand . App.Singleton
					$ OccMouseDown (WindowId $ fromIntegral w) ButtonMiddle
				loop rs
			Just (Vk.EventMouseButtonUp w GlfwG.Ms.MouseButton'1) -> do
				atomically . writeTChan cocc
					. App.expand . App.Singleton
					$ OccMouseUp (WindowId $ fromIntegral w) ButtonLeft
				loop rs
			Just (Vk.EventMouseButtonUp w GlfwG.Ms.MouseButton'2) -> do
				atomically . writeTChan cocc
					. App.expand . App.Singleton
					$ OccMouseUp (WindowId $ fromIntegral w) ButtonRight
				loop rs
			Just (Vk.EventMouseButtonUp w GlfwG.Ms.MouseButton'3) -> do
				atomically . writeTChan cocc
					. App.expand . App.Singleton
					$ OccMouseUp (WindowId $ fromIntegral w) ButtonMiddle
				loop rs
			Just (Vk.EventMouseButtonDown _ _) -> loop rs
			Just (Vk.EventMouseButtonUp _ _) -> loop rs
			Just (Vk.EventCursorPosition k x y) -> do
				atomically . writeTChan cocc
					. App.expand . App.Singleton
					$ OccMouseMove (WindowId $ fromIntegral k) (x, y)
				loop rs
			Just (Vk.EventOpenWindow k) -> do
				putStrLn $ "open window: " ++ show k
				atomically . writeTChan cocc
					. App.expand . App.Singleton
					. OccWindowNew . WindowId $ fromIntegral k
				loop rs
			Just (Vk.EventDeleteWindow k) -> do
				putStrLn $ "delete window: " ++ show k
				atomically . writeTChan cocc
					. App.expand . App.Singleton
					. OccDeleteEvent . WindowId $ fromIntegral k
				loop rs

-- RECTANGLES

instances :: Float -> [Vk.Rectangle]
instances tm = let m = calcModel tm in
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

instances2 :: Float -> [Vk.Rectangle]
instances2 tm = let m = calcModel tm in
	[
		Vk.Rectangle (Vk.RectPos . Cglm.Vec2 $ (- 1) :. (- 1) :. NilL)
			(Vk.RectSize . Cglm.Vec2 $ 0.3 :. 0.3 :. NilL)
			(Vk.RectColor . Cglm.Vec4 $ 0.0 :. 1.0 :. 0.0 :. 1.0 :. NilL)
			m,
		Vk.Rectangle (Vk.RectPos . Cglm.Vec2 $ 1 :. 1 :. NilL)
			(Vk.RectSize . Cglm.Vec2 $ 0.6 :. 0.6 :. NilL)
			(Vk.RectColor . Cglm.Vec4 $ 0.0 :. 0.0 :. 1.0 :. 1.0 :. NilL)
			m,
		Vk.Rectangle (Vk.RectPos . Cglm.Vec2 $ 1.5 :. (- 1.5) :. NilL)
			(Vk.RectSize . Cglm.Vec2 $ 0.6 :. 0.3 :. NilL)
			(Vk.RectColor . Cglm.Vec4 $ 1.0 :. 1.0 :. 1.0 :. 1.0 :. NilL)
			m,
		Vk.Rectangle (Vk.RectPos . Cglm.Vec2 $ (- 1.5) :. 1.5 :. NilL)
			(Vk.RectSize . Cglm.Vec2 $ 0.6 :. 0.3 :. NilL)
			(Vk.RectColor . Cglm.Vec4 $ 1.0 :. 0.0 :. 0.0 :. 1.0 :. NilL)
			m
		]

calcModel :: Float -> Vk.RectModel
calcModel tm = Vk.RectModel $ Cglm.rotate Cglm.mat4Identity
		(tm * Cglm.rad 90) (Cglm.Vec3 $ 0 :. 0 :. 1 :. NilL)
