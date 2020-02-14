{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MouseAndTime where

import qualified Data.Set as S

import React
import GuiEv

mouseDown :: ReactG s [MouseBtn]
mouseDown = pick <$> exper (S.singleton $ MouseDown Request)
	where
	pick evs = case S.elems $ S.filter (== MouseDown Request) evs of
		[MouseDown (Occurred mbs)] -> mbs
		_ -> error "never occur"

mouseUp :: ReactG s [MouseBtn]
mouseUp = pick <$> exper (S.singleton $ MouseUp Request)
	where
	pick evs = case S.elems $ S.filter (== MouseUp Request) evs of
		[MouseUp (Occurred mbs)] -> mbs
		_ -> error "never occur"

mouseMove :: ReactG s Point
mouseMove = pick <$> exper (S.singleton $ MouseMove Request)
	where
	pick evs = case S.elems $ S.filter (== MouseMove Request) evs of
		[MouseMove (Occurred pt)] -> pt
		_ -> error "never occur"
