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

import Graphics.UI.GlfwG.Key

main :: IO ()
main = do
	(ip, op, vex) <- atomically
		$ (,,) <$> newTChan <*> newTChan <*> newTVar (Vk.Extent2d 0 0)
	_ <- forkIO $ mainloop
		(writeTChan ip) (isEmptyTChan op, readTChan op) (readTVar vex)
	useCairo ip op vex

mainloop ::
	(Command -> STM ()) -> (STM Bool, STM Event) -> STM Vk.Extent2d -> IO ()
mainloop ip (ne, op) ex = do
	_ <- forkIO . forever $ threadDelay 5000 >> atomically (ip GetEvent)
	_ <- forkIO $ forever do
		threadDelay 1000000
		t <- getZonedTime
		atomically . ip . SetViewAsTexture $ View [ln, shwt t]
	va <- atomically $ newTVar (pi / 2)
	fix \go -> do
		threadDelay 2000
		a <- atomically $ readTVar va
		o <- atomically $ ex >>= \e -> do
			ip $ DrawRect (viewProj a e) rectangles
			bool (Just <$> op) (pure Nothing) =<< ne
		case o of
			Nothing -> go
			Just EventEnd -> putStrLn "THE WORLD ENDS"
			Just (EventKeyUp Key'Q) -> ew >> go
			Just EventDeleteWindow -> ew >> go
			Just EventGamepadButtonAPressed -> ew >> go
			Just (EventKeyDown Key'Left) -> rtt L 1 va >> go
			Just (EventKeyRepeat Key'Left) -> rtt L 1 va >> go
			Just (EventKeyDown Key'Right) -> rtt R 1 va >> go
			Just (EventKeyRepeat Key'Right) -> rtt R 1 va >> go
			Just (EventGamepadAxisLeftX lx) -> rtt R lx va >> go
			Just ev -> putStrLn (show ev ++ " occur") >> go
	where
	ln = expand . Singleton
		$ Line' (Color 127 170 127) 32 (100, 100) (400, 400)
	shwt t = expand . Singleton
		$ Text' blue "sans" 200
			(50, 600) (T.pack $ formatTime defaultTimeLocale "%T" t)
	ew = atomically $ ip EndWorld
	rtt :: LR -> Float -> TVar Angle -> IO ()
	rtt d a va = atomically
		$ modifyTVar va (lr (+) subtract d $ pi * realToFrac a / 100)

data LR = L | R deriving Show

lr :: a -> a -> LR -> a
lr l r = \case L -> l; R -> r

viewProj :: Angle -> Vk.Extent2d -> ViewProj
viewProj (Angle a) sce = ViewProj {
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

rectangles :: [Rectangle]
rectangles = [
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
