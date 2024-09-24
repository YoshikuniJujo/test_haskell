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
	useCairo, Event(..), Command(..), ViewProj(..),

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

import Trial.Followbox.ViewType
import Data.OneOfThem

import Graphics.UI.GlfwG.Key as GlfwG.Ky

main :: IO ()
main = do
	(ip, op, vex) <- atomically
		$ (,,) <$> newTChan <*> newTChan <*> newTVar (Vk.Extent2d 0 0)
	_ <- forkIO $ mainloop
		(writeTChan ip, unGetTChan ip EndWorld)
		(isEmptyTChan op, readTChan op) (readTVar vex)
	useCairo ip op vex

mainloop ::
	(Command -> STM (), STM()) -> (STM Bool, STM Event) ->
	STM Vk.Extent2d -> IO ()
mainloop (ip, ew) (ne, op) ex = do
	ta <- atomically $ newTVar (pi / 2)
	_ <- forkIO $ forever do
		threadDelay 5000
		atomically $ ip GetEvent
	_ <- forkIO $ forever do
		threadDelay 1000000
		t <- getZonedTime
		atomically do
			ip $ SetViewAsTexture
				(View [	expand . Singleton $ Line' (Color 127 127 127) 4 (10, 10) (100, 100),
					expand . Singleton $ Text' blue "sans" 200 (50, 600) (T.pack $ formatTime defaultTimeLocale "%T" t)
					])
	fix \loop -> do
		threadDelay 2000
		a <- atomically $ readTVar ta
		o <- atomically do
			e0 <- ex
			ip $ DrawRect (uniformBufferObject a e0) instancesMore
			bool (Just <$> op) (pure Nothing) =<< ne
		case o of
			Nothing -> loop
			Just EventEnd -> putStrLn "THE WORLD ENDS"
			Just (EventKeyDown Key'Left) -> do
				atomically $ modifyTVar ta (+ pi * 1 / 100)
				loop
			Just (EventKeyRepeating Key'Left) -> do
				atomically $ modifyTVar ta (+ pi * 1 / 100)
				loop
			Just (EventKeyDown Key'Right) -> do
				atomically $ modifyTVar ta (subtract $ pi * 1 / 100)
				loop
			Just (EventKeyRepeating Key'Right) -> do
				atomically $ modifyTVar ta (subtract $ pi * 1 / 100)
				loop
			Just (EventKeyDown ky) -> do
				putStrLn ("KEY DOWN      : " ++ show ky)
				loop
			Just (EventKeyUp Key'Q) -> do
				putStrLn ("KEY UP       : " ++ show Key'Q)
				atomically ew
				loop
			Just (EventKeyUp ky) -> do
				putStrLn ("KEY UP       : " ++ show ky)
				loop
			Just (EventKeyRepeating ky) -> do
				putStrLn ("KEY REPEATING: " ++ show ky)
				loop
			Just EventDeleteWindow -> do
				putStrLn $ "delete window"
				atomically $ ip EndWorld
				loop
			Just (EventGamepadAxisLeftX lx) -> do
				atomically $ modifyTVar ta (subtract $ realToFrac (pi * lx / 100))
				loop
			Just EventGamepadButtonAPressed -> do
				putStrLn $ "EventGamepadButtonAPressed"
				atomically $ ip EndWorld
				loop

uniformBufferObject :: Angle -> Vk.Extent2d -> ViewProj
uniformBufferObject (Angle a) sce = ViewProj {
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

newtype Angle = Angle Double deriving (Show, Eq, Ord, Num, Real, Fractional, Floating)

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
