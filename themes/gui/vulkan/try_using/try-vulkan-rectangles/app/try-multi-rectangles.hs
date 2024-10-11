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
body f inp (oute, outp) ext = do
	tm0 <- getCurrentTime
	atomically $ inp OpenWindow
	atomically $ inp OpenWindow
	atomically $ inp OpenWindow
	($ rects1) $ fix \loop rs -> do
		threadDelay 10000
		now <- getCurrentTime
		let	tm = realToFrac $ now `diffUTCTime` tm0
		o <- atomically do
			e0 <- ext 0
			e1 <- ext 1
			inp . Draw $ M.fromList [
				(0, ((bool (viewProj e0) def f), (rs tm))),
				(1, ((bool (viewProj e1) def f), (rects2 tm)))
				]
			bool (Just <$> outp) (pure Nothing) =<< oute
		case o of
			Nothing -> loop rs
			Just (EventKeyDown w GlfwG.Ky.Key'D) -> do
				putStrLn ("KEY DOWN: " ++ show w ++ " d")
				atomically . inp $ DestroyWindow w
				loop rs
			Just EventEnd -> putStrLn "THE WORLD ENDS"
			Just (EventKeyDown w ky) -> do
				putStrLn ("KEY DOWN: " ++ show w ++ " " ++ show ky)
				loop rs
			Just (EventKeyUp w ky) -> do
				putStrLn ("KEY UP  : " ++ show w ++ " " ++ show ky)
				loop rs
			Just (EventMouseButtonDown 0 GlfwG.Ms.MouseButton'1) ->
				loop rects1
			Just (EventMouseButtonDown 0 GlfwG.Ms.MouseButton'2) ->
				loop rects2
			Just (EventMouseButtonDown _ _) -> loop rs
			Just (EventMouseButtonUp _ _) -> loop rs
			Just (EventCursorPosition k x y) ->
				putStrLn ("position: " ++ show k ++ " " ++ show (x, y)) >>
				loop rs
			Just (EventOpenWindow k) -> do
				putStrLn $ "open window: " ++ show k
				loop rs
			Just (EventDeleteWindow k) -> do
				putStrLn $ "delete window: " ++ show k
				atomically . inp $ DestroyWindow k
				loop rs

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
