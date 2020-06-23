{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MonadicFrp.XFieldHandle.Mouse (
	-- * Handle Mouse
	handleMouse ) where

import Foreign.C.Types (CInt)
import Data.Type.Set (Set(Nil), Singleton, (:-))
import Data.OneOrMore (singleton, expand, (>-))
import Data.Time (DiffTime)
import System.Exit (exitSuccess)

import MonadicFrp (EvOccs)
import MonadicFrp.Handle (Handle')
import MonadicFrp.Event.Mouse (
	Occurred(..), MouseEv, MouseDown, MouseUp, MouseMove, MouseBtn(..) )
import Field (
	Field, Event(..), flushField, closeField,
	withNextEvent, withNextEventTimeout', isDeleteEvent )

---------------------------------------------------------------------------

-- HANDLE MOUSE
-- EVENT TO EV

---------------------------------------------------------------------------
-- HANDLE MOUSE
---------------------------------------------------------------------------

handleMouse :: Maybe DiffTime -> Field -> Handle' IO MouseEv
handleMouse Nothing f _rqs = withNextEvent f $ eventToEv f
handleMouse (Just prd) f _rqs = withNextEventTimeout' f
	(round $ prd * 1000000) $ maybe (pure Nothing) (eventToEv f)

---------------------------------------------------------------------------
-- EVENT TO EV
---------------------------------------------------------------------------

type MouseMoveDown = MouseMove :- MouseDown :- 'Nil
type MouseMoveUp = MouseMove :- MouseUp :- 'Nil

eventToEv :: Field -> Event -> IO (Maybe (EvOccs MouseEv))
eventToEv f = \case
	ButtonEvent { ev_event_type = 4, ev_button = eb, ev_x = x, ev_y = y }
		| Just b <- btn eb -> pure . Just . expand $ omd x y [b]
	ButtonEvent { ev_event_type = 5, ev_button = eb, ev_x = x, ev_y = y }
		| Just b <- btn eb -> pure . Just . expand $ omu x y [b]
	MotionEvent { ev_x = x, ev_y = y } -> pure . Just . expand $ omm x y
	ExposeEvent {} -> Nothing <$ flushField f
	DestroyWindowEvent {} -> closeField f >> exitSuccess
	ev	| isDeleteEvent f ev -> pure . Just $ expand ode
		| otherwise -> pure Nothing
	where
	omd :: CInt -> CInt -> [MouseBtn] -> EvOccs MouseMoveDown
	omd x y bs = OccMouseDown bs >- omm x y
	omu :: CInt -> CInt -> [MouseBtn] -> EvOccs MouseMoveUp
	omu x y bs = OccMouseUp bs >- omm x y
	omm :: CInt -> CInt -> EvOccs (Singleton MouseMove)
	omm x y = singleton $ OccMouseMove (fromIntegral x, fromIntegral y)
	ode = singleton OccDeleteEvent
	btn = \case
		1 -> Just MLeft; 2 -> Just MMiddle; 3 -> Just MRight
		4 -> Just MUp; 5 -> Just MDown; _ -> Nothing
