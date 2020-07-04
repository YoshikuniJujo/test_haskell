{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Moffy.XFieldHandle.Mouse where

import Foreign.C.Types
import Data.Type.Set
import Data.OneOrMore
import Data.Time
import System.Exit

import Control.Moffy
import Control.Moffy.Handle hiding (expand)
import Moffy.Event.Mouse
import Field

handleMouse :: Maybe DiffTime -> Field -> Handle' IO MouseEv
handleMouse Nothing f _rqs = withNextEvent f $ eventToEv f
handleMouse (Just prd) f _rqs = withNextEventTimeout' f
	(round $ prd * 1000000) $ maybe (pure Nothing) (eventToEv f)

eventToEv :: Field -> Event -> IO (Maybe (EvOccs MouseEv))
eventToEv f = \case
	ButtonEvent { ev_event_type = 4, ev_button = eb, ev_x = x, ev_y = y }
		| Just b <- btn eb -> pure . Just . expand $ omd x y [b]
	ButtonEvent { ev_event_type = 5, ev_button = eb, ev_x = x, ev_y = y }
		| Just b <- btn eb -> pure . Just . expand $ omu x y [b]
	MotionEvent { ev_x = x, ev_y = y } -> pure . Just . expand $ omm x y
	ExposeEvent {} -> Nothing <$ flushField f
	DestroyWindowEvent {} -> closeField f >> exitSuccess
	ev	| isDeleteEvent f ev -> pure . Just . expand $ singleton OccDeleteEvent -- Nothing <$ destroyField f
		| otherwise -> pure Nothing
	where
	btn = \case
		1 -> Just MLeft; 2 -> Just MMiddle; 3 -> Just MRight
		4 -> Just MUp; 5 -> Just MDown; _ -> Nothing
	omd :: CInt -> CInt -> [MouseBtn] -> EvOccs (MouseMove :- MouseDown :- 'Nil)
	omd x y bs = OccMouseDown bs >- omm x y
	omu :: CInt -> CInt -> [MouseBtn] -> EvOccs (MouseMove :- MouseUp :- 'Nil)
	omu x y bs = OccMouseUp bs >- omm x y
	omm :: CInt -> CInt -> EvOccs (Singleton MouseMove)
	omm x y = singleton $ OccMouseMove (fromIntegral x, fromIntegral y)
