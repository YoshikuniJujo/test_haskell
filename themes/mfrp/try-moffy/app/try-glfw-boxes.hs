{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
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
import Control.Moffy.Event.Mouse (MouseDown(..), MouseBtn(..), pattern OccMouseDown)
import Control.Moffy.Event.Mouse.DefaultWindow
import Control.Moffy.Handle.Time
import Data.Traversable
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
	TVar [EvOccs (DeleteEvent :- MouseDown :- 'Nil)] ->
	TVar (Map WindowId Glfw.Window) -> TVar Glfw.MouseButtonState ->
	Handle' IO (DeleteEvent :- MouseDown :- 'Nil)
handleDelete teos ti2w tbs _ = do
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
			polling teos i2w (keys i2w) tbs
		Just eo -> pure $ Just eo

polling ::
	TVar [EvOccs (DeleteEvent :- MouseDown :- 'Nil)] ->
	Map WindowId Glfw.Window -> [WindowId] -> TVar Glfw.MouseButtonState ->
	IO (Maybe (EvOccs (DeleteEvent :- MouseDown :- 'Nil)))
polling teos i2w is tbs = do
	e NE.:| es <- pollAll i2w is tbs
	atomically $ modifyTVar teos (++ es)
	pure $ Just e

pollAll ::
	Map WindowId Glfw.Window -> [WindowId] -> TVar Glfw.MouseButtonState ->
	IO (NE.NonEmpty (EvOccs (DeleteEvent :- MouseDown :- 'Nil)))
pollAll i2w is tbs = doUntilNotEmpty do
	threadDelay 100000
	Glfw.pollEvents
	catMaybes <$> for is \i ->
		App.merge' <$> getOccDelete i2w i <*> getOccMouseDown i2w i ButtonRight tbs

getOccDelete :: Map WindowId Glfw.Window ->
	WindowId -> IO (Maybe (EvOccs (DeleteEvent :- 'Nil)))
getOccDelete i2w i = do
	let	w = i2w ! i
	b <- Glfw.windowShouldClose w
	pure if b then Just . App.Singleton $ OccDeleteEvent i else Nothing

getOccMouseDown :: Map WindowId Glfw.Window ->
	WindowId -> MouseBtn -> TVar Glfw.MouseButtonState -> IO (Maybe (EvOccs (MouseDown :- 'Nil)))
getOccMouseDown i2w i b tbs0 = do
	let	w = i2w ! i
		b' = btnToButton b
	bs <- Glfw.getMouseButton w b'
	bs0 <- atomically do
		s <- readTVar tbs0
		writeTVar tbs0 bs
		pure s
	pure case (bs0, bs) of
		(Glfw.MouseButtonState'Released, Glfw.MouseButtonState'Pressed) -> 
			Just . App.Singleton $ OccMouseDown i b
		_ -> Nothing

btnToButton :: MouseBtn -> Glfw.MouseButton
btnToButton ButtonLeft = Glfw.MouseButton'1
btnToButton ButtonRight = Glfw.MouseButton'2
btnToButton _ = error "yet"

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
	tbs <- atomically $ newTVar Glfw.MouseButtonState'Released
	Glfw.init
	(print =<<) . ($ (Nothing :: Maybe WindowId)) $ interpretReactSt @_
		@(WindowNew :- LoadDefaultWindow :- StoreDefaultWindow :- DeleteEvent :- MouseDown :- 'Nil)
		@(WindowNew :- LoadDefaultWindow :- StoreDefaultWindow :- DeleteEvent :- MouseDown :- 'Nil)
		(retrySt $ 
			handleWindowNew' undefined i2w undefined `mergeSt`
			handleDefaultWindow `mergeSt` liftHandle' (handleDelete teos i2w tbs)) do
		i <- adjust windowNew
		adjust $ storeDefaultWindow i
		adjust $ deleteEvent `first` rightClick
	Glfw.terminate
