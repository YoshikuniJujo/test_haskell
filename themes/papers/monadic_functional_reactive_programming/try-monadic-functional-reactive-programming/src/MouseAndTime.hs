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
