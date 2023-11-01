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

import UseCairo

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

import Data.Map qualified as M

import Control.Moffy.Event.Window
import Control.Moffy.Event.CalcTextExtents qualified as CTE

main :: IO ()
main = run_ action

action :: Flag "f" '["flat"] "BOOL" "flat or not" Bool ->
	Cmd "Draw Rectangles" ()
action f = liftIO do
	(inp, outp) <- atomically $ (,) <$> newTChan <*> newTChan
	vext <- atomically $ newTVar M.empty
	forkIO $ untilEnd (get f) (
		(writeTChan inp, (isEmptyTChan outp, readTChan outp)),
		readTVarOr (Vk.Extent2d 0 0) vext )
	rectangles2 inp outp vext

untilEnd :: Bool -> (
	(Command Int -> STM (), (STM Bool, STM (Event Int))),
	Int -> STM Vk.Extent2d ) -> IO ()
untilEnd f ((inp, (oute, outp)), ext) = do
	tm0 <- getCurrentTime
	atomically $ inp OpenWindow

	atomically . inp . CalcTextLayoutExtent
		$ CTE.CalcTextExtentsReq (WindowId 0) "serif" 30 "Hello, world!"

	forkIO $ forever do
		threadDelay 5000
		atomically $ inp GetEvent

	($ instances) $ fix \loop rs -> do
		threadDelay 2000
		now <- getCurrentTime
		let	tm = realToFrac $ now `diffUTCTime` tm0
--		print =<< atomically (ext 0)
		o <- atomically do
			e0 <- ext 0
			e1 <- ext 1
			inp . Draw $ M.fromList [
				(0, ((bool (uniformBufferObject e0) def f), instances' 1024 1024 e0)),
--				(0, ((bool (uniformBufferObject e0) def f), (rs tm))),
				(1, ((bool (uniformBufferObject e1) def f), (instances2 tm)))
				]
			bool (Just <$> outp) (pure Nothing) =<< oute
		case o of
			Nothing -> loop rs
			Just EventEnd -> putStrLn "THE WORLD ENDS"
			Just (EventKeyDown w ky) -> do
				putStrLn ("KEY DOWN: " ++ show w ++ " " ++ show ky)
				loop rs
			Just (EventKeyUp w ky) -> do
				putStrLn ("KEY UP  : " ++ show w ++ " " ++ show ky)
				loop rs
			Just (EventMouseButtonDown 0 GlfwG.Ms.MouseButton'1) ->
				loop instances
			Just (EventMouseButtonDown 0 GlfwG.Ms.MouseButton'2) ->
				loop instances2
			Just (EventMouseButtonDown _ _) -> loop rs
			Just (EventMouseButtonUp _ _) -> loop rs
			Just (EventCursorPosition k x y) ->
--				putStrLn ("position: " ++ show k ++ " " ++ show (x, y)) >>
				loop rs
			Just (EventOpenWindow k) -> do
				putStrLn $ "open window: " ++ show k
				loop rs
			Just (EventDeleteWindow k) -> do
				putStrLn $ "delete window: " ++ show k
				atomically . inp $ DestroyWindow k
				loop rs
			Just (EventTextLayoutExtentResult ex) -> do
				print ex
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
