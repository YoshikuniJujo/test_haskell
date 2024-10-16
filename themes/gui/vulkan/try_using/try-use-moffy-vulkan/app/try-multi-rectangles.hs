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

import Rectangles

import Control.Monad
import Control.Monad.Fix
import Control.Concurrent
import Control.Concurrent.STM
import Data.List.Length
import Data.Default
import Data.Bool
import Data.Time

import Gpu.Vulkan qualified as Vk
import Gpu.Vulkan.Cglm qualified as Cglm

import Options.Declarative
import Control.Monad.Trans

import Graphics.UI.GlfwG.Mouse qualified as GlfwG.Ms
import Graphics.UI.GlfwG.Key qualified as GlfwG.Ky

import Data.Map qualified as M

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
		now <- getCurrentTime
		let	tm = realToFrac $ now `diffUTCTime` tm0
		o <- atomically do
			e0 <- ex 0
			e1 <- ex 1
			ip . Draw $ M.fromList [
				(0, ((bool (uniformBufferObject e0) def f), (rs tm))),
				(1, ((bool (uniformBufferObject e1) def f), (instances2 tm)))
				]
			bool (Just <$> op) (pure Nothing) =<< oe
		case o of
			Nothing -> go rs
			Just EventEnd -> putStrLn "THE WORLD ENDS"
			Just (EventMouseButtonDown 0 GlfwG.Ms.MouseButton'1) ->
				go rects1
			Just (EventMouseButtonDown 0 GlfwG.Ms.MouseButton'2) ->
				go instances2
			Just (EventMouseButtonDown _ _) -> go rs
			Just (EventMouseButtonUp _ _) -> go rs
			Just (EventCursorPosition _k _x _y) ->
--				putStrLn ("position: " ++ show k ++ " " ++ show (x, y)) >>
				go rs
			Just (EventOpenWindow k) -> do
				putStrLn $ "open window: " ++ show k
				go rs
			Just (EventKeyDown w GlfwG.Ky.Key'D) ->
				atomically (ip $ DestroyWindow w) >> go rs
			Just ev -> do
				putStrLn $ "unknown event occur: " ++ show ev
				go rs

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

-- RECTANGLES

rects1 :: Float -> [Rectangle]
rects1 tm = let
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
