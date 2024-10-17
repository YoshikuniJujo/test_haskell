{-# LANGUAGE PackageImports, ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans
import Control.Concurrent
import Control.Concurrent.STM
import Data.Default
import Data.List.Length
import Data.Map qualified as M
import Data.Bool
import Data.Time
import Options.Declarative

import Graphics.UI.GlfwG.Mouse qualified as GlfwG.Ms
import Graphics.UI.GlfwG.Key qualified as GlfwG.Ky

import Gpu.Vulkan qualified as Vk
import Gpu.Vulkan.Cglm qualified as Cglm

import Rectangles

----------------------------------------------------------------------
--
-- * MAIN
-- * BODY
-- * RECTANGLES
--
----------------------------------------------------------------------

-- MAIN

main :: IO ()
main = run_ realMain

realMain :: Flag "f" '["flat"] "BOOL" "flat or not" Bool ->
	Cmd "Draw Rectangles" ()
realMain f = liftIO do
	(ip, op, vex) <- atomically
		$ (,,) <$> newTChan <*> newTChan <*> newTVar M.empty
	_ <- forkIO . forever
		$ threadDelay 10000 >> atomically (writeTChan ip GetEvent)
	_ <- forkIO $ body (get f) (writeTChan ip)
		(isEmptyTChan op, readTChan op) (lookupOr (Vk.Extent2d 0 0) vex)
	rectangles ip op vex
	where
	lookupOr d t k = M.lookup k <$> readTVar t >>= maybe (pure d) readTVar

-- BODY

body :: Bool -> (Command Int -> STM ()) -> (STM Bool, STM (Event Int)) ->
	(Int -> STM Vk.Extent2d) -> IO ()
body f ip (oe, op) ex = getCurrentTime >>= \tm0 -> do
	atomically $ ip OpenWindow
	atomically $ ip OpenWindow
	atomically $ ip OpenWindow
	($ rects1) $ fix \go rs -> do
		threadDelay 10000
		tm <- realToFrac . (`diffUTCTime` tm0) <$> getCurrentTime
		o <- atomically do
			(e0, e1) <- (,) <$> ex 0 <*> ex 1
			ip . Draw $ M.fromList [
				(0, (bool (viewProj e0) def f, rs tm)),
				(1, (bool (viewProj e1) def f, rects2 tm)) ]
			bool (Just <$> op) (pure Nothing) =<< oe
		case o of
			Nothing -> go rs
			Just EventEnd -> pure ()
			Just (EventOpenWindow _) -> go rs
			Just (EventDeleteWindow k) ->
				atomically (ip $ DestroyWindow k) >> go rs
			Just (EventKeyDown w GlfwG.Ky.Key'D) ->
				atomically (ip $ DestroyWindow w) >> go rs
			Just (EventKeyDown _ _) -> go rs
			Just (EventKeyUp _ _) -> go rs
			Just (EventMouseButtonDown 0 GlfwG.Ms.MouseButton'1) ->
				go rects1
			Just (EventMouseButtonDown 0 GlfwG.Ms.MouseButton'2) ->
				go rects2
			Just (EventMouseButtonDown _ _) -> go rs
			Just (EventMouseButtonUp _ _) -> go rs
			Just (EventCursorPosition _ _ _) -> go rs

viewProj :: Vk.Extent2d -> ViewProjection
viewProj Vk.Extent2d {
	Vk.extent2dWidth = fromIntegral -> w,
	Vk.extent2dHeight = fromIntegral -> h } = ViewProjection {
	viewProjectionView = Cglm.lookat
		(Cglm.Vec3 $ 2 :. 2 :. 2 :. NilL)
		(Cglm.Vec3 $ 0 :. 0 :. 0 :. NilL)
		(Cglm.Vec3 $ 0 :. 0 :. 1 :. NilL),
	viewProjectionProj = Cglm.modifyMat4 1 1 negate
		$ Cglm.perspective (Cglm.rad 45) (w / h) 0.1 10 }

-- RECTANGLES

rects1 :: Float -> [Rectangle]
rects1 tm = [
	Rectangle (rectPos (- 1) (- 1))
		(rectSize 0.3 0.3) (rectColor 1.0 0.0 0.0 1.0) m,
	Rectangle (rectPos 1 1)
		(rectSize 0.2 0.2) (rectColor 0.0 1.0 0.0 1.0) m,
	Rectangle (rectPos 1.5 (- 1.5))
		(rectSize 0.3 0.6) (rectColor 0.0 0.0 1.0 1.0) m,
	Rectangle (rectPos (- 1.5) 1.5)
		(rectSize 0.6 0.3) (rectColor 1.0 1.0 1.0 1.0) m ]
	where m = rotModel tm

rects2 :: Float -> [Rectangle]
rects2 tm = [
	Rectangle (rectPos (- 1) (- 1))
		(rectSize 0.3 0.3) (rectColor 0.0 1.0 0.0 1.0) m,
	Rectangle (rectPos 1 1)
		(rectSize 0.6 0.6) (rectColor 0.0 0.0 1.0 1.0) m,
	Rectangle (rectPos 1.5 (- 1.5))
		(rectSize 0.6 0.3) (rectColor 1.0 1.0 1.0 1.0) m,
	Rectangle (rectPos (- 1.5) 1.5)
		(rectSize 0.6 0.3) (rectColor 1.0 0.0 0.0 1.0) m ]
	where m = rotModel tm

rotModel :: Float -> RectModel
rotModel tm = RectModel $ Cglm.rotate Cglm.mat4Identity
		(tm * Cglm.rad 90) (Cglm.Vec3 $ 0 :. 0 :. 1 :. NilL)
