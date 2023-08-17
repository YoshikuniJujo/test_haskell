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
import Data.Maybe
import Data.List.NonEmpty qualified as NE
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

handleDelete ::
	TVar [EvOccs (DeleteEvent :- 'Nil)] ->
	TVar (Map WindowId Glfw.Window) -> Handle' IO (Singleton DeleteEvent)
handleDelete teos ti2w _ = do
	meo <- atomically do
		eoa <- readTVar teos
		case eoa of
			[] -> pure Nothing
			eo : eos -> do
				writeTVar teos eos
				pure $ Just eo
	case meo of
		Nothing -> do
			i2w <- atomically $ readTVar ti2w
			polling teos i2w (keys i2w)
		Just eo -> pure $ Just eo

polling ::
	TVar [EvOccs (DeleteEvent :- 'Nil)] ->
	Map WindowId Glfw.Window -> [WindowId] ->
	IO (Maybe (EvOccs (DeleteEvent :- 'Nil)))
polling teos i2w is = do
	e NE.:| es <- pollAll i2w is
	atomically $ modifyTVar teos (++ es)
	pure $ Just e

pollAll ::
	Map WindowId Glfw.Window -> [WindowId] ->
	IO (NE.NonEmpty (EvOccs (DeleteEvent :- 'Nil)))
pollAll i2w is = doUntilNotEmpty do
	let	ws = (i2w !) <$> is
	threadDelay 100000
	Glfw.pollEvents
	bs <- Glfw.windowShouldClose `mapM` ws
	pure . catMaybes $ (\f -> zipWith f bs is) \b i -> if b
		then Just . App.Singleton $ OccDeleteEvent i else Nothing

doUntil_ :: Monad m => m Bool -> m ()
doUntil_ act = do
	b <- act
	if b then pure () else doUntil_ act

doUntil :: Monad m => m (Maybe a) -> m a
doUntil act = do
	mx <- act
	case mx of
		Nothing -> doUntil act
		Just x -> pure x

doUntilNotEmpty :: Monad m => m [a] -> m (NE.NonEmpty a)
doUntilNotEmpty act = do
	xs <- act
	case xs of
		[] -> doUntilNotEmpty act
		x : xs -> pure $ x NE.:| xs

getMouseDown ::
	TVar (Map WindowId Glfw.Window) -> WindowId ->
	IO (Maybe (EvOccs (MouseDown :- 'Nil)))
getMouseDown i2w i = undefined

main :: IO ()
main = do
	teos <- atomically $ newTVar []
	i2w <- atomically $ newTVar empty
	Glfw.init
	(print =<<) . ($ (Nothing :: Maybe WindowId)) $ interpretReactSt @_
		@(WindowNew :- LoadDefaultWindow :- StoreDefaultWindow :- DeleteEvent :- 'Nil)
		@(WindowNew :- LoadDefaultWindow :- StoreDefaultWindow :- DeleteEvent :- 'Nil)
		(retrySt $ 
			handleWindowNew' undefined i2w undefined `mergeSt`
			handleDefaultWindow `mergeSt` liftHandle' (handleDelete teos i2w)) do
		i <- adjust windowNew
		adjust $ storeDefaultWindow i
		adjust deleteEvent
	Glfw.terminate
