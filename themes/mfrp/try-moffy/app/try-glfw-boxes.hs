{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Concurrent
import Control.Concurrent.STM hiding (retry)
import Control.Moffy
import Control.Moffy.Run
import Control.Moffy.Handle
import Control.Moffy.Handle.DefaultWindow
import Control.Moffy.Event.Window
import Control.Moffy.Event.DefaultWindow
import Control.Moffy.Event.Delete hiding (deleteEvent)
import Control.Moffy.Event.Delete.DefaultWindow
import Control.Moffy.Event.Mouse (MouseDown(..))
import Control.Moffy.Event.Mouse.DefaultWindow
import Control.Moffy.Handle.Time
import Data.Map hiding (adjust)
import Data.Type.Set
import Data.OneOrMore hiding (expand)
import Data.OneOrMoreApp qualified as App

import Trial.Boxes

import Graphics.UI.GLFW qualified as Glfw

handleWindowNew ::
	TVar WindowId ->
	TVar (Map WindowId Glfw.Window) -> TVar (Map Glfw.Window WindowId) ->
	Handle' IO (Singleton WindowNew)
handleWindowNew niv i2w w2i (unSingleton -> WindowNewReq) = do
	putStrLn "handleWindowNew"
	Just w <- Glfw.createWindow 700 500 "foo" Nothing Nothing
	atomically $ modifyTVar i2w $ insert (WindowId 0) w
	pure . Just . App.Singleton . OccWindowNew $ WindowId 0

handleWindowNew' ::
	TVar WindowId ->
	TVar (Map WindowId Glfw.Window) -> TVar (Map Glfw.Window WindowId) ->
	HandleSt' s IO (Singleton WindowNew)
handleWindowNew' wid i2w w2i = liftHandle' $ handleWindowNew wid i2w w2i

handleDelete :: TVar (Map WindowId Glfw.Window) -> Handle' IO (Singleton DeleteEvent)
handleDelete ti2w _ = do
	i2w <- atomically $ readTVar ti2w
	let	w = i2w ! WindowId 0
	doUntil_ do
		threadDelay 100000
		Glfw.pollEvents
		Glfw.windowShouldClose w
	pure . Just . App.Singleton . OccDeleteEvent $ WindowId 0

doUntil_ :: IO Bool -> IO ()
doUntil_ act = do
	b <- act
	if b then pure () else doUntil_ act

-- getMouseDown :: Glfw.Window -> IO (Maybe (EvOccs (MouseDown :- 'Nil)))
-- getMouseDown w = 

main :: IO ()
main = do
	i2w <- atomically $ newTVar empty
	Glfw.init
	(print =<<) . ($ (Nothing :: Maybe WindowId)) $ interpretReactSt @_
		@(WindowNew :- LoadDefaultWindow :- StoreDefaultWindow :- DeleteEvent :- 'Nil)
		@(WindowNew :- LoadDefaultWindow :- StoreDefaultWindow :- DeleteEvent :- 'Nil)
		(retrySt $ 
			handleWindowNew' undefined i2w undefined `mergeSt`
			handleDefaultWindow `mergeSt` liftHandle' (handleDelete i2w)) do
		i <- adjust windowNew
		adjust $ storeDefaultWindow i
		adjust deleteEvent
	Glfw.terminate
