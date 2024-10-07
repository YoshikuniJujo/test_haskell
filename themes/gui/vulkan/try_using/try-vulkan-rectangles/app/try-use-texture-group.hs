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
	useTextureGroup, Event(..), Command(..), ViewProjection(..),
	Rectangle(..), RectPos(..), RectSize(..), RectColor(..), RectModel(..) )

import Control.Monad
import Control.Monad.Fix
import Control.Concurrent
import Control.Concurrent.STM
import Data.Traversable
import Data.List.Length
import Data.Default
import Data.Bool
import Data.Time
import System.FilePath

import Gpu.Vulkan qualified as Vk
import Gpu.Vulkan.Cglm qualified as Cglm

import Options.Declarative
import Control.Monad.Trans

import Graphics.UI.GlfwG.Mouse qualified as GlfwG.Ms
import Graphics.UI.GLFW qualified as Glfw

import Data.Map qualified as M

import Graphics.UI.GlfwG.Key as GlfwG.Ky

import Codec.Picture qualified as Pct

----------------------------------------------------------------------
--
-- * MAIN
-- * BODY
-- * RECTANGLES
--
----------------------------------------------------------------------

-- MAIN

main :: IO ()
main = run_ $ realMain rectangles

realMain ::
	Rectangles ->
	Flag "f" '["flat"] "BOOL" "flat or not" Bool -> Cmd "Draw Rectangles" ()
realMain rects f = liftIO do
	(ip, op) <- atomically $ (,) <$> newTChan <*> newTChan
	(a, vex) <- atomically $ (,) <$> newTVar 0 <*> newTVar M.empty
	_ <- forkIO $ controller a ip
	_ <- forkIO . forever
		$ threadDelay 10000 >> atomically (writeTChan ip GetEvent)
	tpctf <- atomically newTChan
	_ <- forkIO . forever $ setPicture ip tpctf
	_ <- forkIO $ body rects (get f) a
		(writeTChan ip) (isEmptyTChan op, readTChan op)
		(lookupOr (Vk.Extent2d 0 0) vex) tpctf
	useTextureGroup ip op vex =<< either error Pct.convertRGBA8
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
setPicture ip tpctf = atomically (readTChan tpctf) >>= \(w, fp) ->
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

body :: Rectangles -> Bool -> TVar Angle ->
	(Command Int -> STM ()) -> (STM Bool, STM (Event Int)) ->
	(Int -> STM Vk.Extent2d) -> TChan (Int, String) -> IO ()
body rects f ta ip (oe, op) ex tpctf = getCurrentTime >>= \tm0 ->
	atomically ((,) <$> newTVar False <*> newTVar "") >>= \(tip, vtxt) -> do
	atomically $ ip OpenWindow
	($ initRects) $ fix \go rs -> threadDelay 10000 >> do
		tm <- realToFrac . (`diffUTCTime` tm0) <$> getCurrentTime
		o <- atomically do
			a <- readTVar ta
			ip =<< mkDraws f a ex rs tm
			bool (Just <$> op) (pure Nothing) =<< oe
		atomically (readTVar tip) >>= bool
			(processOutput rects o go rs ip tip)
			(inputTxFilePath o go rs tip tpctf vtxt)
	where
	initRects = M.map (M.! GlfwG.Ms.MouseButton'1) rectangles

mkDraws :: (Ord k, Num k, Enum k, Monad m) =>
	Bool -> Angle -> (k -> m Vk.Extent2d) -> M.Map k (Vk.Extent2d -> t -> [Rectangle]) -> t -> m (Command k)
mkDraws f a ex rs tm = Draw . M.fromList
	<$> for [0 .. 1] \k -> mkDraw f a ex (rs M.! k) tm k

mkDraw :: Monad m =>
	Bool -> Angle -> (a -> m Vk.Extent2d) -> (Vk.Extent2d -> t -> b) -> t -> a -> m (a, (ViewProjection, b))
mkDraw f a ex rs tm k = do
	e <- ex k
	pure (k, ((bool (viewProj a e) def f), rs e tm))

viewProj :: Angle -> Vk.Extent2d -> ViewProjection
viewProj (Angle a) sce = ViewProjection {
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

processOutput ::
	Rectangles ->
	Maybe (Event Int) -> (M.Map Int (Vk.Extent2d -> Float -> [Rectangle]) -> IO ()) ->
	M.Map Int (Vk.Extent2d -> Float -> [Rectangle]) -> (Command Int -> STM ()) -> TVar Bool ->
	IO ()
processOutput rects o loop rs inp tinput =
		case o of
			Nothing -> loop rs
			Just EventEnd -> putStrLn "THE WORLD ENDS"
			Just (EventKeyDown _w Key'O) -> atomically (inp OpenWindow) >> loop rs
			Just (EventKeyDown w Key'D) -> do
				putStrLn $ "delete window: " ++ show w
				atomically . inp $ DestroyWindow w
				loop rs
			Just (EventKeyDown w ky) -> do
				putStrLn ("KEY DOWN: " ++ show w ++ " " ++ show ky)
				loop rs
			Just (EventKeyUp w Key'Q) -> do
				putStrLn ("KEY UP  : " ++ show w ++ " " ++ show Key'Q)
				atomically . inp $ DestroyWindow w
				loop rs
			Just (EventKeyUp _ Key'T) -> do
				putStrLn "T"
				atomically $ writeTVar tinput True
--				atomically $ writeTChan tbgn ()
--				threadDelay 2000000
				loop rs
			Just (EventKeyUp w ky) -> do
				putStrLn ("KEY UP  : " ++ show w ++ " " ++ show ky)
				loop rs
			Just (EventMouseButtonDown n b) ->
				loop $ maybe rs (\r -> M.insert n r rs) (rectangle rects n b)
			Just (EventMouseButtonUp _ _) -> loop rs
			Just (EventCursorPosition _k _x _y) ->
--				putStrLn ("position: " ++ show k ++ " " ++ show (x, y)) >>
				loop rs
			Just (EventOpenWindow k) -> do
				putStrLn $ "open window: " ++ show k
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

rectangle :: Rectangles -> Int -> GlfwG.Ms.MouseButton ->
	Maybe (Vk.Extent2d -> Float -> [Rectangle])
rectangle rs n b = M.lookup b =<< M.lookup n rs

inputTxFilePath :: Maybe (Event k) ->
	(t -> IO b) -> t -> TVar Bool -> TChan (k, String) -> TVar [Char] -> IO b
inputTxFilePath o loop rs tinput tbgn vtxt =
	case o of
		Just (EventKeyDown w Key'Enter) -> do
			atomically $ writeTVar tinput False
			atomically $ writeTChan tbgn . (w ,) . reverse =<< readTVar vtxt
			atomically $ writeTVar vtxt ""
			loop rs
		Just (EventKeyDown _w ky) -> do
			atomically $ modifyTVar vtxt (maybe id (:) $ keyToChar ky)
			loop rs
		_ -> do
			putStrLn . reverse =<< atomically (readTVar vtxt)
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
	(Key'Space, ' ') ]

-- RECTANGLES

type Rectangles = M.Map Int (M.Map GlfwG.Ms.MouseButton (Vk.Extent2d -> Float -> [Rectangle]))

rectangles :: Rectangles
rectangles = M.fromList [
	(0, M.fromList [
		(GlfwG.Ms.MouseButton'1, \e _ -> instances' 1024 1024 e),
		(GlfwG.Ms.MouseButton'2, \e _ -> instances' 1024 1024 e) ]),
	(1, M.fromList [
		(GlfwG.Ms.MouseButton'1, \_ t -> instances t),
		(GlfwG.Ms.MouseButton'2, \_ t -> instances2 t) ]) ]

instances :: Float -> [Rectangle]
instances tm = let m = calcModel' tm in
	[
--		Rectangle (RectPos . Cglm.Vec2 $ (- 1) :. (- 1) :. NilL)
		Rectangle (RectPos . Cglm.Vec2 $ (- 2) :. (- 2) :. NilL)
--			(RectSize . Cglm.Vec2 $ 0.3 :. 0.3 :. NilL)
			(RectSize . Cglm.Vec2 $ 4 :. 4 :. NilL)
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
instances2 tm = let m = calcModel' tm in
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

instances' :: Float -> Float -> Vk.Extent2d -> [Rectangle]
instances' w h ex = let m = calcModel2' w h ex in
	[
--		Rectangle (RectPos . Cglm.Vec2 $ (- 1) :. (- 1) :. NilL)
		Rectangle (RectPos . Cglm.Vec2 $ (- 2) :. (- 2) :. NilL)
--			(RectSize . Cglm.Vec2 $ 0.3 :. 0.3 :. NilL)
			(RectSize . Cglm.Vec2 $ 4 :. 4 :. NilL)
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

calcModel' :: Float -> RectModel
calcModel' tm = RectModel $ Cglm.rotate
	Cglm.mat4Identity (tm * Cglm.rad 90) (Cglm.Vec3 $ 0 :. 0 :. 1 :. NilL)

calcModel2' :: Float -> Float -> Vk.Extent2d -> RectModel
calcModel2' w0 h0 Vk.Extent2d { Vk.extent2dWidth = w, Vk.extent2dHeight = h } =
	RectModel $ Cglm.scale Cglm.mat4Identity
		(Cglm.Vec3 $ (w0 / fromIntegral w) :. (h0 / fromIntegral h) :. 1 :. NilL)
