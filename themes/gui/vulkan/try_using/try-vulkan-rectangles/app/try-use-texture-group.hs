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

import Control.Arrow
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans
import Control.Concurrent
import Control.Concurrent.STM
import Data.Traversable
import Data.Default
import Data.List.Length
import Data.Map qualified as M
import Data.Bool
import Data.Time
import System.FilePath
import Codec.Picture qualified as Pct
import Options.Declarative

import Graphics.UI.GLFW qualified as Glfw
import Graphics.UI.GlfwG.Key as GlfwG.Ky
import Graphics.UI.GlfwG.Mouse qualified as GlfwG.Ms

import Gpu.Vulkan qualified as Vk
import Gpu.Vulkan.Cglm qualified as Glm

import UseTextureGroup (
	useTextureGroup, Event(..), Command(..), ViewProjection(..),
	Rectangle(..), RectPos(..), RectSize(..), RectColor(..), RectModel(..) )

----------------------------------------------------------------------
--
-- * MAIN
-- * BODY
-- * RECTANGLES
--
----------------------------------------------------------------------

-- MAIN

main :: IO ()
main = run_ $ realMain rectTable

realMain ::
	RectangleTable ->
	Flag "f" '["flat"] "BOOL" "flat or not" Bool -> Cmd "Draw Rectangles" ()
realMain rt f = liftIO do
	(cmd, ev) <- atomically $ (,) <$> newTChan <*> newTChan
	(a, vex) <- atomically $ (,) <$> newTVar 0 <*> newTVar M.empty
	_ <- forkIO $ controller a cmd
	_ <- forkIO . forever
		$ threadDelay 10000 >> atomically (writeTChan cmd GetEvent)
	txf <- atomically newTChan
	_ <- forkIO . forever $ setPicture cmd txf
	_ <- forkIO $ body rt (get f) a
		(writeTChan cmd) (isEmptyTChan ev, readTChan ev)
		(lookupOr (Vk.Extent2d 0 0) vex) txf
	useTextureGroup cmd ev vex =<< either error Pct.convertRGBA8
		<$> Pct.readImage "../../../../../files/images/texture.jpg"
	where
	lookupOr d t k = M.lookup k <$> readTVar t >>= maybe (pure d) readTVar

newtype Angle = Angle Double deriving (Show, Eq, Ord, Num, Real, Fractional)

controller :: TVar Angle -> TChan (Command Int) -> IO ()
controller a inp = fix \go -> (>> go) . (threadDelay 50000 >>)
	$ Glfw.getGamepadState Glfw.Joystick'1 >>= \case
		Nothing -> pure ()
		Just (Glfw.GamepadState gb ga) -> do
			when (gb Glfw.GamepadButton'A ==
				Glfw.GamepadButtonState'Pressed)
				. atomically $ writeTChan inp EndWorld
			atomically $ modifyTVar a (+ realToFrac
				(pi * ga Glfw.GamepadAxis'LeftX / 100))

setPicture :: TChan (Command k) -> TChan (k, String) -> IO ()
setPicture ip txf = atomically (readTChan txf) >>= \(w, fp) ->
	maybe (pure ()) (void . atomically . writeTChan ip . SetPicture w) =<<
	(either error Pct.convertRGBA8 <$>) <$> case fp of
		"texture" -> Just <$> Pct.readImage (imgDir </> "texture.jpg")
		"viking room" ->
			Just <$> Pct.readImage (mdlDir </> "viking_room.png")
		"flower" -> Just <$> Pct.readImage (imgDir </> "flower.jpg")
		"dice" -> Just <$> Pct.readImage (imgDir </> "saikoro.png")
		_ -> pure Nothing

imgDir, mdlDir :: FilePath
imgDir = "../../../../../files/images"; mdlDir = "../../../../../files/models"

-- BODY

body :: RectangleTable -> Bool -> TVar Angle ->
	(Command Int -> STM ()) -> (STM Bool, STM (Event Int)) ->
	(Int -> STM Vk.Extent2d) -> TChan (Int, String) -> IO ()
body rt f ta cmd (nev, ev) ex txf = getCurrentTime >>= \tm0 ->
	atomically ((,) <$> newTVar False <*> newTVar "") >>= \(tip, vtxt) -> do
	atomically $ cmd OpenWindow
	($ irs) $ fix \go rs -> threadDelay 10000 >> do
		tm <- realToFrac . (`diffUTCTime` tm0) <$> getCurrentTime
		e <- atomically do
			a <- readTVar ta
			cmd =<< draw f a rs ex tm
			bool (Just <$> ev) (pure Nothing) =<< nev
		atomically (readTVar tip) >>= bool
			(processEvent rt cmd e tip go rs)
			(inputTxFilePath e tip vtxt txf >> go rs)
	where irs = M.map (M.! GlfwG.Ms.MouseButton'1) rt

draw :: (Ord k, Num k, Enum k, Monad m) =>
	Bool -> Angle -> M.Map k (Vk.Extent2d -> t -> [Rectangle]) ->
	(k -> m Vk.Extent2d) -> t -> m (Command k)
draw f a rs ex tm =
	Draw . M.fromList <$> for [0 .. 1] \k -> (<$> ex k) \e ->
	(k, ((bool (viewProj a e) def f), (rs M.! k) e tm))

viewProj :: Angle -> Vk.Extent2d -> ViewProjection
viewProj (Angle a) Vk.Extent2d {
	Vk.extent2dWidth = fromIntegral -> w,
	Vk.extent2dHeight = fromIntegral -> h } = ViewProjection {
	viewProjectionView = Glm.lookat
		(Glm.Vec3 $ cx :. cy :. 3 :. NilL)
		(Glm.Vec3 $ 0 :. 0 :. 0 :. NilL)
		(Glm.Vec3 $ 0 :. 0 :. 1 :. NilL),
	viewProjectionProj = Glm.modifyMat4 1 1 negate
		$ Glm.perspective (Glm.rad 45) (w / h) 0.1 10 }
	where (cx, cy) = realToFrac *** realToFrac $ (cos &&& sin) a

processEvent :: RectangleTable ->
	(Command Int -> STM ()) -> Maybe (Event Int) -> TVar Bool ->
	(M.Map Int (Vk.Extent2d -> Float -> [Rectangle]) -> IO ()) ->
	M.Map Int (Vk.Extent2d -> Float -> [Rectangle]) -> IO ()
processEvent rt ((atomically .) -> cmd) o tip go rs = case o of
	Nothing -> go rs
	Just (EventOpenWindow _) -> go rs
	Just (EventDeleteWindow k) -> cmd (DestroyWindow k) >> go rs
	Just (EventKeyDown w Key'D) -> cmd (DestroyWindow w) >> go rs
	Just (EventKeyUp w Key'Q) -> cmd (DestroyWindow w) >> go rs
	Just EventNeedRedraw -> go rs; Just EventEnd -> pure ()
	Just (EventKeyDown _ Key'O) -> cmd OpenWindow >> go rs
	Just (EventKeyUp _ Key'T) -> atomically (writeTVar tip True) >> go rs
	Just (EventMouseButtonDown n b) ->
		go $ maybe rs (\r -> M.insert n r rs) (rects n b)
	Just (EventKeyDown _ _) -> go rs; Just (EventKeyUp _ _) -> go rs
	Just (EventMouseButtonUp _ _) -> go rs
	Just (EventCursorPosition _ _ _) -> go rs
	where rects n b = M.lookup b =<< M.lookup n rt

inputTxFilePath :: Maybe (Event k) ->
	TVar Bool -> TVar [Char] -> TChan (k, String) -> IO ()
inputTxFilePath o tip txt txf = case o of
	Just (EventKeyDown w Key'Enter) -> atomically do
		writeTChan txf . (w ,) . reverse =<< readTVar txt
		writeTVar tip False; writeTVar txt ""
	Just (EventKeyDown _ ky) -> do
		atomically $ modifyTVar txt (maybe id (:) $ keyToChar ky)
		putStrLn . reverse =<< atomically (readTVar txt)
	_ -> pure ()

keyToChar :: Key -> Maybe Char
keyToChar = (`lookup` [
	(Key'A, 'a'), (Key'B, 'b'), (Key'C, 'c'), (Key'D, 'd'), (Key'E, 'e'),
	(Key'F, 'f'), (Key'G, 'g'), (Key'H, 'h'), (Key'I, 'i'), (Key'J, 'j'),
	(Key'K, 'k'), (Key'L, 'l'), (Key'M, 'm'), (Key'N, 'n'), (Key'O, 'o'),
	(Key'P, 'p'), (Key'Q, 'q'), (Key'R, 'r'), (Key'S, 's'), (Key'T, 't'),
	(Key'U, 'u'), (Key'V, 'v'), (Key'W, 'w'), (Key'X, 'x'), (Key'Y, 'y'),
	(Key'Z, 'z'), (Key'Space, ' ') ])

-- RECTANGLES

rectTable :: RectangleTable
rectTable = M.fromList [
	(0, M.fromList [
		(GlfwG.Ms.MouseButton'1, \e _ -> scaled 1024 1024 e),
		(GlfwG.Ms.MouseButton'2, \e _ -> scaled 512 512 e) ]),
	(1, M.fromList [
		(GlfwG.Ms.MouseButton'1, \_ t -> rotated t),
		(GlfwG.Ms.MouseButton'2, \_ t -> rotated2 t) ]) ]

type RectangleTable = M.Map Int
	(M.Map GlfwG.Ms.MouseButton (Vk.Extent2d -> Float -> [Rectangle]))

scaled :: Float -> Float -> Vk.Extent2d -> [Rectangle]
scaled w h ex = let m = scale w h ex in [
	Rectangle (RectPos . Glm.Vec2 $ (- 2) :. (- 2) :. NilL)
		(RectSize . Glm.Vec2 $ 4 :. 4 :. NilL)
		(RectColor . Glm.Vec4 $ 1.0 :. 0.0 :. 0.0 :. 1.0 :. NilL) m,
	Rectangle (RectPos . Glm.Vec2 $ 1 :. 1 :. NilL)
		(RectSize . Glm.Vec2 $ 0.2 :. 0.2 :. NilL)
		(RectColor . Glm.Vec4 $ 0.0 :. 1.0 :. 0.0 :. 1.0 :. NilL) m,
	Rectangle (RectPos . Glm.Vec2 $ 1.5 :. (- 1.5) :. NilL)
		(RectSize . Glm.Vec2 $ 0.3 :. 0.6 :. NilL)
		(RectColor . Glm.Vec4 $ 0.0 :. 0.0 :. 1.0 :. 1.0 :. NilL) m,
	Rectangle (RectPos . Glm.Vec2 $ (- 1.5) :. 1.5 :. NilL)
		(RectSize . Glm.Vec2 $ 0.6 :. 0.3 :. NilL)
		(RectColor . Glm.Vec4 $ 1.0 :. 1.0 :. 1.0 :. 1.0 :. NilL) m ]

rotated :: Float -> [Rectangle]
rotated tm = let m = rotate tm in [
	Rectangle (RectPos . Glm.Vec2 $ (- 2) :. (- 2) :. NilL)
		(RectSize . Glm.Vec2 $ 4 :. 4 :. NilL)
		(RectColor . Glm.Vec4 $ 1.0 :. 0.0 :. 0.0 :. 1.0 :. NilL) m,
	Rectangle (RectPos . Glm.Vec2 $ 1 :. 1 :. NilL)
		(RectSize . Glm.Vec2 $ 0.2 :. 0.2 :. NilL)
		(RectColor . Glm.Vec4 $ 0.0 :. 1.0 :. 0.0 :. 1.0 :. NilL) m,
	Rectangle (RectPos . Glm.Vec2 $ 1.5 :. (- 1.5) :. NilL)
		(RectSize . Glm.Vec2 $ 0.3 :. 0.6 :. NilL)
		(RectColor . Glm.Vec4 $ 0.0 :. 0.0 :. 1.0 :. 1.0 :. NilL) m,
	Rectangle (RectPos . Glm.Vec2 $ (- 1.5) :. 1.5 :. NilL)
		(RectSize . Glm.Vec2 $ 0.6 :. 0.3 :. NilL)
		(RectColor . Glm.Vec4 $ 1.0 :. 1.0 :. 1.0 :. 1.0 :. NilL) m ]

rotated2 :: Float -> [Rectangle]
rotated2 tm = let m = rotate tm in [
	Rectangle (RectPos . Glm.Vec2 $ (- 1) :. (- 1) :. NilL)
		(RectSize . Glm.Vec2 $ 0.3 :. 0.3 :. NilL)
		(RectColor . Glm.Vec4 $ 0.0 :. 1.0 :. 0.0 :. 1.0 :. NilL) m,
	Rectangle (RectPos . Glm.Vec2 $ 1 :. 1 :. NilL)
		(RectSize . Glm.Vec2 $ 0.6 :. 0.6 :. NilL)
		(RectColor . Glm.Vec4 $ 0.0 :. 0.0 :. 1.0 :. 1.0 :. NilL) m,
	Rectangle (RectPos . Glm.Vec2 $ 1.5 :. (- 1.5) :. NilL)
		(RectSize . Glm.Vec2 $ 0.6 :. 0.3 :. NilL)
		(RectColor . Glm.Vec4 $ 1.0 :. 1.0 :. 1.0 :. 1.0 :. NilL) m,
	Rectangle (RectPos . Glm.Vec2 $ (- 1.5) :. 1.5 :. NilL)
		(RectSize . Glm.Vec2 $ 0.6 :. 0.3 :. NilL)
		(RectColor . Glm.Vec4 $ 1.0 :. 0.0 :. 0.0 :. 1.0 :. NilL) m ]

scale :: Float -> Float -> Vk.Extent2d -> RectModel
scale w0 h0 Vk.Extent2d {
	Vk.extent2dWidth = fromIntegral -> w,
	Vk.extent2dHeight = fromIntegral -> h } = RectModel $ Glm.scale
	Glm.mat4Identity (Glm.Vec3 $ (w0 / w) :. (h0 / h) :. 1 :. NilL)

rotate :: Float -> RectModel
rotate tm = RectModel $ Glm.rotate
	Glm.mat4Identity (tm * Glm.rad 90) (Glm.Vec3 $ 0 :. 0 :. 1 :. NilL)
