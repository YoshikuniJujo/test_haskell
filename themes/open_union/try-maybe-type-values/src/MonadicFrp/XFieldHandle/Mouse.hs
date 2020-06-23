{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MonadicFrp.XFieldHandle.Mouse (
	-- * Handle Mouse
	handleMouse ) where

import Foreign.C.Types (CInt)
import Data.Type.Set (Set(Nil), Singleton, (:-))
import Data.UnionSet (singleton, expand, (>-))
import Data.Time (DiffTime)
import System.Exit (exitSuccess)

import MonadicFrp (EvOccs)
import MonadicFrp.Handle (Handle')
import MonadicFrp.Event.Mouse (
	Occurred(..), MouseEv, MouseDown, MouseUp, MouseMove, MouseBtn(..) )
import Field (
	Field, Event(..), Button, flushField, closeField,
	withNextEvent, withNextEventTimeout', isDeleteEvent )

---------------------------------------------------------------------------

-- HANDLE MOUSE
-- EVENT TO EV

---------------------------------------------------------------------------
-- HANDLE MOUSE
---------------------------------------------------------------------------

handleMouse :: Maybe DiffTime -> Field -> Handle' IO MouseEv
handleMouse Nothing f _reqs = withNextEvent f $ eventToEv f
handleMouse (Just prd) f _reqs =
	withNextEventTimeout'' f prd $ maybe (pure Nothing) (eventToEv f)
	where
	withNextEventTimeout'' :: Field -> DiffTime -> (Maybe Event -> IO a) ->  IO a
	withNextEventTimeout'' f = withNextEventTimeout' f . round . (* 1000000)

---------------------------------------------------------------------------
-- EVENT TO EV
---------------------------------------------------------------------------

eventToEv :: Field -> Event -> IO (Maybe (EvOccs MouseEv))
eventToEv f = \case
	ButtonEvent {
		ev_event_type = 4, ev_button = eb,
		ev_x = x, ev_y = y } | Just b <- button eb ->
			pure . Just . expand $ mouseDownOcc x y [b]
	ButtonEvent {
		ev_event_type = 5, ev_button = eb,
		ev_x = x, ev_y = y } | Just b <- button eb ->
			pure . Just . expand $ mouseUpOcc x y [b]
	MotionEvent { ev_x = x, ev_y = y } ->
		pure . Just . expand $ mouseMoveOcc x y
	ExposeEvent {} -> Nothing <$ flushField f
	DestroyWindowEvent {} -> closeField f >> exitSuccess
	ev	| isDeleteEvent f ev ->
			pure . Just . expand $ singleton OccDeleteEvent
		| otherwise -> pure Nothing

mouseDownOcc ::
	CInt -> CInt -> [MouseBtn] -> EvOccs (MouseMove :- MouseDown :- 'Nil)
mouseDownOcc x y bs =OccMouseDown bs >- mouseMoveOcc x y

mouseUpOcc ::
	CInt -> CInt -> [MouseBtn] -> EvOccs (MouseMove :- MouseUp :- 'Nil)
mouseUpOcc x y bs = OccMouseUp bs >- mouseMoveOcc x y

mouseMoveOcc :: CInt -> CInt -> EvOccs (Singleton MouseMove)
mouseMoveOcc x y = singleton $ OccMouseMove (fromIntegral x, fromIntegral y)

button :: Button -> Maybe MouseBtn
button = \case
	1 -> Just MLeft; 2 -> Just MMiddle; 3 -> Just MRight
	4 -> Just MUp; 5 -> Just MDown; _ -> Nothing
