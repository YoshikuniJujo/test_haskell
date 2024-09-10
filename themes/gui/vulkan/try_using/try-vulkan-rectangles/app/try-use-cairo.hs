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

import UseCairo (
	useCairo, Event(..), Command(..), ViewProjection(..),

	Rectangle(..), RectPos(..), RectSize(..), RectColor(..), RectModel(..)

	)

import Control.Monad
import Control.Monad.Fix
import Control.Concurrent
import Control.Concurrent.STM
import Data.List.Length
import Data.Bool
import Data.Time
import Data.Text qualified as T

import Gpu.Vulkan qualified as Vk
import Gpu.Vulkan.Cglm qualified as Cglm

import Data.Map qualified as M

import Control.Moffy.Event.Window
import Control.Moffy.Event.CalcTextExtents qualified as CTE

import Trial.Followbox.ViewType
import Data.OneOfThem

import Graphics.UI.GlfwG.Key as GlfwG.Ky

main :: IO ()
main = do
	a <- newAngle
	(inp, outp) <- atomically $ (,) <$> newTChan <*> newTChan
	vext <- atomically $ newTVar M.empty
	_ <- forkIO $ untilEnd a (
		((writeTChan inp, \k -> unGetTChan inp (DestroyWindow k)), (isEmptyTChan outp, readTChan outp)),
		readTVarOr (Vk.Extent2d 0 0) vext )
--	_ <- forkIO $ controller a inp
	useCairo inp outp vext

readTVarOr :: Ord k => a -> TVar (M.Map k (TVar a)) -> k -> STM a
readTVarOr d mp k = do
	mv <- (M.lookup k) <$> readTVar mp
	case mv of
		Nothing -> pure d
		Just v -> readTVar v

untilEnd :: TVar Angle -> (
	((Command () -> STM (), () -> STM()), (STM Bool, STM (Event ()))),
	() -> STM Vk.Extent2d ) -> IO ()
untilEnd ta (((inp, dw), (oute, outp)), ext) = do
	atomically $ inp OpenWindow

	atomically . inp . CalcTextLayoutExtent
		$ CTE.CalcTextExtentsReq (WindowId 0) "serif" 30 "Hello, world!"

	_ <- forkIO $ forever do
		threadDelay 5000
		atomically $ inp GetEvent

	_ <- forkIO $ forever do
		threadDelay 1000000
		t <- getZonedTime
		atomically do
			inp $ Draw2
				(View [	expand . Singleton $ Line' (Color 127 127 127) 4 (10, 10) (100, 100),
					expand . Singleton $ Text' blue "sans" 200 (50, 600) (T.pack $ formatTime defaultTimeLocale "%T" t)
					])

	fix \loop -> do
		threadDelay 2000
		a <- atomically $ readTVar ta
		o <- atomically do
			e0 <- ext ()
			inp . Draw $ M.fromList
				[((), (uniformBufferObject a e0, instancesMore))]
			bool (Just <$> outp) (pure Nothing) =<< oute
		case o of
			Nothing -> loop
			Just EventEnd -> putStrLn "THE WORLD ENDS"
			Just (EventKeyDown _w Key'Left) -> do
				atomically $ modifyTVar ta (+ pi * 1 / 100)
				loop
			Just (EventKeyRepeating _w Key'Left) -> do
				atomically $ modifyTVar ta (+ pi * 1 / 100)
				loop
			Just (EventKeyDown _w Key'Right) -> do
				atomically $ modifyTVar ta (subtract $ pi * 1 / 100)
				loop
			Just (EventKeyRepeating _w Key'Right) -> do
				atomically $ modifyTVar ta (subtract $ pi * 1 / 100)
				loop
			Just (EventKeyDown w ky) -> do
				putStrLn ("KEY DOWN      : " ++ show w ++ " " ++ show ky)
				loop
			Just (EventKeyUp w Key'Q) -> do
				putStrLn ("KEY UP       : " ++ show w ++ " " ++ show Key'Q)
				atomically $ dw w
				loop
			Just (EventKeyUp w ky) -> do
				putStrLn ("KEY UP       : " ++ show w ++ " " ++ show ky)
				loop
			Just (EventKeyRepeating w ky) -> do
				putStrLn ("KEY REPEATING: " ++ show w ++ " " ++ show ky)
				loop
			Just (EventMouseButtonDown _ _) -> loop
			Just (EventMouseButtonUp _ _) -> loop
			Just (EventCursorPosition _k _x _y) ->
--				putStrLn ("position: " ++ show k ++ " " ++ show (x, y)) >>
				loop
			Just (EventOpenWindow k) -> do
				putStrLn $ "open window: " ++ show k
				loop
			Just (EventDeleteWindow k) -> do
				putStrLn $ "delete window: " ++ show k
				atomically . inp $ DestroyWindow k
				loop
			Just (EventTextLayoutExtentResult ex) -> do
				print ex
				loop
			Just EventNeedRedraw -> do
				putStrLn "EVENT NEED REDRAW"
				loop
			Just (EventGamepadAxisLeftX lx) -> do
				atomically $ modifyTVar ta (subtract $ realToFrac (pi * lx / 100))
				loop
			Just EventGamepadButtonAPressed -> do
				putStrLn $ "EventGamepadButtonAPressed"
				atomically $ inp EndWorld
				loop

uniformBufferObject :: Angle -> Vk.Extent2d -> ViewProjection
uniformBufferObject (Angle a) sce = ViewProjection {
	viewProjectionView = Cglm.lookat
		(Cglm.Vec3 $ lax :. lay :. 1.7 :. NilL)
		(Cglm.Vec3 $ 0 :. 0 :. 0 :. NilL)
		(Cglm.Vec3 $ 0 :. 0 :. (- 1) :. NilL),
	viewProjectionProj = id -- Cglm.modifyMat4 1 1 negate
		$ Cglm.perspective
			(Cglm.rad 90)
			(fromIntegral (Vk.extent2dWidth sce) /
				fromIntegral (Vk.extent2dHeight sce)) 0.1 10 }
	where
	lax = realToFrac $ cos a; lay = realToFrac $ sin a

instancesMore :: [Rectangle]
instancesMore = [
	Rectangle (RectPos . Cglm.Vec2 $ (- 1.8) :. (- 1.8) :. NilL)
		(RectSize . Cglm.Vec2 $ 3.6 :. 3.6 :. NilL)
		(RectColor . Cglm.Vec4 $ 1.0 :. 0.0 :. 0.0 :. 1.0 :. NilL)
		m1,
	Rectangle (RectPos . Cglm.Vec2 $ 1 :. 1 :. NilL)
		(RectSize . Cglm.Vec2 $ 0.8 :. 0.8 :. NilL)
		(RectColor . Cglm.Vec4 $ 0.0 :. 1.0 :. 0.0 :. 1.0 :. NilL)
		m1,
	Rectangle (RectPos . Cglm.Vec2 $ 0.5 :. (- 0.5) :. NilL)
		(RectSize . Cglm.Vec2 $ 0.7 :. 0.9 :. NilL)
		(RectColor . Cglm.Vec4 $ 0.0 :. 0.0 :. 1.0 :. 1.0 :. NilL)
		m2,
	Rectangle (RectPos . Cglm.Vec2 $ (- 1.0) :. 0.7 :. NilL)
		(RectSize . Cglm.Vec2 $ 0.9 :. 0.5 :. NilL)
		(RectColor . Cglm.Vec4 $ 1.0 :. 1.0 :. 1.0 :. 1.0 :. NilL)
		m2 ]
	where
	m1 = RectModel $ Cglm.scale Cglm.mat4Identity (Cglm.Vec3 $ 1 :. 1 :. 1 :. NilL)
	tr1 = Cglm.translate Cglm.mat4Identity (Cglm.Vec3 $ 0 :. (- 0.3) :. 0.5 :. NilL)
	m2 = RectModel $
		Cglm.scale tr1 (Cglm.Vec3 $ 1 :. 1 :. 1 :. NilL)

newtype Angle = Angle Double deriving (Show, Eq, Ord, Num, Real, Fractional, Floating)

newAngle :: IO (TVar Angle)
newAngle = atomically $ newTVar (pi / 2)
