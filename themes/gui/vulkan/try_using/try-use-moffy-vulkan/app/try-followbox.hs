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

import UseCairo qualified as Vk
import KeyToXKey

import Control.Monad.Fix
import Control.Concurrent
import Control.Concurrent.STM hiding (retry)
import Data.Default
import Data.List.Length
import Data.Map qualified as M
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
	(ccmd@(writeTChan -> cmd), cev@(readTChan -> ev), ex) <- atomically
		$ (,,) <$> newTChan <*> newTChan <*> newTVar M.empty
	let	ext = lookupOr (Vk.Extent2d 0 0) ex
	ff $ threadDelay 20000 >> atomically (cmd Vk.GetEvent)
	(crqs@(readTChan -> rqs), cocc@(writeTChan -> occ))
		<- atomically $ (,) <$> newTChan <*> newTChan
	ffa $ mffReqsToVk cmd cocc =<< rqs

	c' <- atomically newTChan
	e <- atomically newTChan
	v <- atomically . newTVar $ View []

	_ <- forkIO $ untilEnd e crqs cocc c' ((cmd, ev), ext) v
	_ <- forkIO $ runFollowboxGen crqs cocc "firefox" Nothing c' do
		i <- waitFor $ adjust windowNew
		_ <- waitFor . adjust $ setCursorFromName i Default
		waitFor . adjust $ storeDefaultWindow i
		M.singleton i <$%> adjustSig (followbox i)
		waitFor . adjust $ windowDestroy i
	Vk.rectangles ccmd cev ex
	where
	f = void . forkIO . void
	ff = void . forkIO . forever; ffa = ff . atomically

lookupOr :: Ord k => a -> TVar (M.Map k (TVar a)) -> k -> STM a
lookupOr d mp k = maybe (pure d) readTVar . M.lookup k =<< readTVar mp

-- SEND REQUEST/EVENT TO VULKAN/MOFFY

mffReqsToVk :: (Vk.Command Int -> STM ()) -> TChan (EvOccs GuiEv) -> EvReqs GuiEv -> STM ()
mffReqsToVk inp cocc rqs = do
	case project rqs of
		Nothing -> pure ()
		Just WindowNewReq -> do
			inp Vk.OpenWindow
	case project rqs of
		Nothing -> pure ()
		Just (WindowDestroyReq i@(WindowId k)) -> do
			do
				inp . Vk.DestroyWindow $ fromIntegral k
				writeTChan cocc . App.expand . App.Singleton $ OccWindowDestroy i
	case project rqs of
		Nothing -> pure ()
		Just (CalcTextExtentsReq wid fnm fsz txt) -> pure ()
	case project rqs of
		Nothing -> pure ()
		Just (SetCursorFromNameReq wid nc) -> do
			do
				writeTChan cocc . App.expand . App.Singleton
					$ OccSetCursorFromName wid nc Success
	case project rqs of
		Nothing -> pure ()
		Just r@(CTE.CalcTextExtentsReq _ _ _ _) ->
			inp $ Vk.CalcTextLayoutExtent r

untilEnd :: TChan () -> TChan (EvReqs GuiEv) -> TChan (EvOccs GuiEv) ->
	TChan (M.Map WindowId View) ->
	((Vk.Command Int -> STM (), STM (Vk.Event Int)), Int -> STM Vk.Extent2d) ->
	TVar View -> IO ()
untilEnd e cow cocc c' ((inp, outp), ext) tvw = do

	_ <- forkIO . forever $ atomically (readTChan c') >>= \vs -> do
		putStrLn $ "VIEW: " ++ show vs
		atomically . writeTVar tvw $ vs M.! WindowId 0
		e0 <- atomically $ ext 0
		atomically . inp $ Vk.SetPicture (vs M.! WindowId 0)
		atomically . inp $ Vk.Draw (M.fromList [(0, (def, rects 1024 1024 e0))])

	fix \loop -> do
		threadDelay 500
		o <- atomically do Just <$> outp
--			bool (Just <$> outp) (pure Nothing) =<< oute
		case o of
			Nothing -> loop
			Just Vk.EventEnd -> putStrLn "THE WORLD ENDS" >> atomically (writeTChan e ())
			Just (Vk.EventKeyDown w ky) -> do
				putStrLn $ "KEY DOWN: " ++ show w ++ " " ++ show ky
				atomically . writeTChan cocc
					. App.expand . App.Singleton
					. OccKeyDown (WindowId $ fromIntegral w) $ keyToXKey ky
				loop
			Just (Vk.EventKeyUp w ky) -> do
				putStrLn $ "KEY UP  : " ++ show w ++ " " ++ show ky
				loop
			Just (Vk.EventMouseButtonDown w GlfwG.Ms.MouseButton'1) -> do
				putStrLn "BUTTON LEFT DOWN"
				atomically . writeTChan cocc
					. App.expand . App.Singleton
					$ OccMouseDown (WindowId $ fromIntegral w) ButtonLeft
				loop
			Just (Vk.EventMouseButtonDown w GlfwG.Ms.MouseButton'2) -> do
				putStrLn "BUTTON RIGHT DOWN"
				atomically . writeTChan cocc
					. App.expand . App.Singleton
					$ OccMouseDown (WindowId $ fromIntegral w) ButtonRight
				loop
			Just (Vk.EventMouseButtonDown w GlfwG.Ms.MouseButton'3) -> do
				atomically . writeTChan cocc
					. App.expand . App.Singleton
					$ OccMouseDown (WindowId $ fromIntegral w) ButtonMiddle
				loop
			Just (Vk.EventMouseButtonUp w GlfwG.Ms.MouseButton'1) -> do
				atomically . writeTChan cocc
					. App.expand . App.Singleton
					$ OccMouseUp (WindowId $ fromIntegral w) ButtonLeft
				loop
			Just (Vk.EventMouseButtonUp w GlfwG.Ms.MouseButton'2) -> do
				atomically . writeTChan cocc
					. App.expand . App.Singleton
					$ OccMouseUp (WindowId $ fromIntegral w) ButtonRight
				loop
			Just (Vk.EventMouseButtonUp w GlfwG.Ms.MouseButton'3) -> do
				atomically . writeTChan cocc
					. App.expand . App.Singleton
					$ OccMouseUp (WindowId $ fromIntegral w) ButtonMiddle
				loop
			Just (Vk.EventMouseButtonDown _ _) -> loop
			Just (Vk.EventMouseButtonUp _ _) -> loop
			Just (Vk.EventCursorPosition k x y) -> do
				atomically . writeTChan cocc
					. App.expand . App.Singleton
					$ OccMouseMove (WindowId $ fromIntegral k) (x, y)
				loop
			Just (Vk.EventOpenWindow k) -> do
				putStrLn $ "open window: " ++ show k
				atomically . writeTChan cocc
					. App.expand . App.Singleton
					. OccWindowNew . WindowId $ fromIntegral k
				loop
			Just (Vk.EventDeleteWindow k) -> do
				putStrLn $ "delete window: " ++ show k
				atomically . writeTChan cocc
					. App.expand . App.Singleton
					. OccDeleteEvent . WindowId $ fromIntegral k
				loop
			Just (Vk.EventTextLayoutExtentResult ex) -> do
				putStrLn $ "EventTextLayoutExtentResult: " ++ show ex
				atomically . writeTChan cocc
					. App.expand $ App.Singleton ex
				loop
			Just Vk.EventNeedRedraw -> do
				putStrLn "EVENT NEED REDRAW"
				e0 <- atomically $ ext 0
				atomically . inp $ Vk.Draw (M.fromList [(0, (def, rects 1024 1024 e0))])
				loop

-- RECTANGLES

rects :: Float -> Float -> Vk.Extent2d -> [Vk.Rectangle]
rects w h ex = let m = model w h ex in
	[
		Vk.Rectangle (Vk.RectPos . Cglm.Vec2 $ (- 2) :. (- 2) :. NilL)
			(Vk.RectSize . Cglm.Vec2 $ 4 :. 4 :. NilL)
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


model :: Float -> Float -> Vk.Extent2d -> Vk.RectModel
model w0 h0 Vk.Extent2d { Vk.extent2dWidth = w, Vk.extent2dHeight = h } =
	Vk.RectModel $ Cglm.scale Cglm.mat4Identity
		(Cglm.Vec3 $ (w0 / fromIntegral w) :. (h0 / fromIntegral h) :. 1 :. NilL)
