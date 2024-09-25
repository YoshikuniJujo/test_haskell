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

import UseTextureGroup (
	rectangles2, Event(..), Command(..), ViewProjection(..),

	Rectangle(..), RectPos(..), RectSize(..), RectColor(..),
	RectModel0(..), RectModel1(..), RectModel2(..), RectModel3(..),

	readTVarOr )

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
import Graphics.UI.GLFW qualified as Glfw

import Data.Map qualified as M

import Control.Moffy.Event.Window
import Control.Moffy.Event.CalcTextExtents qualified as CTE

import Trial.Followbox.ViewType
import Data.OneOfThem

import Graphics.UI.GlfwG.Key as GlfwG.Ky

import Codec.Picture

main :: IO ()
main = run_ action

action :: Flag "f" '["flat"] "BOOL" "flat or not" Bool ->
	Cmd "Draw Rectangles" ()
action f = liftIO do
	a <- newAngle
	(inp, outp) <- atomically $ (,) <$> newTChan <*> newTChan
	vext <- atomically $ newTVar M.empty
	_ <- forkIO $ untilEnd (get f) a (
		(writeTChan inp, (isEmptyTChan outp, readTChan outp)),
		readTVarOr (Vk.Extent2d 0 0) vext )
	_ <- forkIO $ controller a inp
	rectangles2 inp outp vext

newtype Angle = Angle Double deriving (Show, Eq, Ord, Num, Real, Fractional)

newAngle :: IO (TVar Angle)
newAngle = atomically $ newTVar 0

controller :: TVar Angle -> TChan (Command Int) -> IO ()
controller a inp = fix \go -> (>> go) $ (threadDelay 100000 >>) do
	cs <- Glfw.getGamepadState Glfw.Joystick'1
	case cs of
		Just (Glfw.GamepadState gb ga) -> do
			when (gb Glfw.GamepadButton'A == Glfw.GamepadButtonState'Pressed)
				. atomically $ writeTChan inp EndWorld
			print $ ga Glfw.GamepadAxis'LeftX
			print $ ga Glfw.GamepadAxis'LeftY
			atomically $ modifyTVar a (+ realToFrac (pi * ga Glfw.GamepadAxis'LeftX / 100))
			print =<< atomically (readTVar a)
		Nothing -> pure ()

untilEnd :: Bool -> TVar Angle -> (
	(Command Int -> STM (), (STM Bool, STM (Event Int))),
	Int -> STM Vk.Extent2d ) -> IO ()
untilEnd f ta ((inp, (oute, outp)), ext) = do
	tbgn <- atomically newTChan
	tpct <- atomically newTChan
	tm0 <- getCurrentTime
	atomically $ inp OpenWindow

	_ <- forkIO $ forever do
		threadDelay 5000
		atomically $ inp GetEvent

	_ <- forkIO $ forever do
		fp <- atomically $ readTChan tbgn
		img <- case fp of
			"texture" -> Just <$> readImage "../../../../../files/images/texture.jpg"
			"viking room" -> Just <$> readImage "../../../../../files/models/viking_room.png"
			"flower" -> Just <$> readImage "../../../../../files/images/flower.jpg"
			"dice" -> Just <$> readImage "../../../../../files/images/saikoro.png"
			_ -> pure Nothing
		let	pct = either error convertRGBA8 <$> img
			{-
--		threadDelay 4000000
		pct <- either error convertRGBA8 <$> readImage "../../../../../files/images/texture.jpg"
		atomically $ writeTChan tpct pct
--		threadDelay 4000000
		atomically $ readTChan tbgn
		pct' <- either error convertRGBA8 <$> readImage "../../../../../files/models/viking_room.png"
		-}
		maybe (pure ()) (atomically . writeTChan tpct) pct

	_ <- forkIO $ forever do
		pct <- atomically $ readTChan tpct
		a <- atomically $ readTVar ta
		atomically do
			e0 <- ext 0
			inp $ DrawPicture pct

	tinput <- atomically $ newTVar False
	vtxt <-  atomically $ newTVar []

	($ instances) $ fix \loop rs -> do
		threadDelay 2000
		a <- atomically $ readTVar ta
		now <- getCurrentTime
		let	tm = realToFrac $ now `diffUTCTime` tm0
--		print =<< atomically (ext 0)
		o <- atomically do
			e0 <- ext 0
			e1 <- ext 1
			inp $ Draw (M.fromList [
				(0, ((bool (uniformBufferObject a e0) def f), instances' 1024 1024 e0)),
--				(0, ((bool (uniformBufferObject e0) def f), (rs tm))),
				(1, ((bool (uniformBufferObject a e1) def f), (instances2 tm)))
				] )
			bool (Just <$> outp) (pure Nothing) =<< oute
		ti <- atomically $ readTVar tinput
		if ti	then processText o loop rs tinput tbgn vtxt
			else processOutput o loop rs inp tbgn tinput

processText o loop rs tinput tbgn vtxt =
	case o of
		Just (EventKeyDown w Key'Enter) -> do
			atomically $ writeTVar tinput False
			atomically $ writeTChan tbgn . reverse =<< readTVar vtxt
			atomically $ writeTVar vtxt ""
			loop rs
		Just (EventKeyDown w ky) -> do
			atomically $ modifyTVar vtxt (maybe id (:) $ keyToChar ky)
			loop rs
		_ -> do
			putStrLn . reverse =<< atomically (readTVar vtxt)
			loop rs

processOutput o loop rs inp tbgn tinput =
		case o of
			Nothing -> loop rs
			Just EventEnd -> putStrLn "THE WORLD ENDS"
			Just (EventKeyDown _w Key'O) -> atomically (inp OpenWindow) >> loop rs
			Just (EventKeyDown w ky) -> do
				putStrLn ("KEY DOWN: " ++ show w ++ " " ++ show ky)
				loop rs
			Just (EventKeyUp w Key'Q) -> do
				putStrLn ("KEY UP  : " ++ show w ++ " " ++ show Key'Q)
				atomically . inp $ DestroyWindow w
				loop rs
			Just (EventKeyUp w Key'T) -> do
				putStrLn "T"
				atomically $ writeTVar tinput True
--				atomically $ writeTChan tbgn ()
--				threadDelay 2000000
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
			Just (EventCursorPosition _k _x _y) ->
--				putStrLn ("position: " ++ show k ++ " " ++ show (x, y)) >>
				loop rs
			Just (EventOpenWindow k) -> do
				putStrLn $ "open window: " ++ show k
				loop rs
			Just (EventKeyDown w Key'D) -> do
				putStrLn $ "delete window: " ++ show w
				atomically . inp $ DestroyWindow w
				loop rs
			Just (EventDeleteWindow k) -> do
				putStrLn $ "delete window: " ++ show k
				atomically . inp $ DestroyWindow k
				putStrLn $ "EventDeleteWindow " ++ show k ++
					": before next loop"
				loop rs
			Just EventNeedRedraw -> do
				putStrLn "EVENT NEED REDRAW"
				loop rs

keyToChar :: Key -> Maybe Char
keyToChar = (`lookup` keyCharTable)

keyCharTable :: [(Key, Char)]
keyCharTable = [
	(Key'C, 'c'),
	(Key'D, 'd'),
	(Key'E, 'e'),
	(Key'F, 'f'),
	(Key'G, 'g'),
	(Key'I, 'i'),
	(Key'K, 'k'),
	(Key'L, 'l'),
	(Key'M, 'm'),
	(Key'N, 'n'),
	(Key'O, 'o'),
	(Key'R, 'r'),
	(Key'T, 't'),
	(Key'U, 'u'),
	(Key'V, 'v'),
	(Key'W, 'w'),
	(Key'X, 'x'),
	(Key'Space, ' ')
	]

uniformBufferObject :: Angle -> Vk.Extent2d -> ViewProjection
uniformBufferObject (Angle a) sce = ViewProjection {
	viewProjectionView = Cglm.lookat
--		(Cglm.Vec3 $ 2 :. 2 :. 2 :. NilL)
		(Cglm.Vec3 $ lax :. lay :. 3 :. NilL)
		(Cglm.Vec3 $ 0 :. 0 :. 0 :. NilL)
		(Cglm.Vec3 $ 0 :. 0 :. 1 :. NilL),
	viewProjectionProj = Cglm.modifyMat4 1 1 negate
		$ Cglm.perspective
			(Cglm.rad 45)
			(fromIntegral (Vk.extent2dWidth sce) /
				fromIntegral (Vk.extent2dHeight sce)) 0.1 10 }
	where
	lax = realToFrac $ cos a; lay = realToFrac $ sin a

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
