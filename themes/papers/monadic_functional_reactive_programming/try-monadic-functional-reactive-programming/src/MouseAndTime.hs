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
		es -> error $ "never occur: " ++ show es

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

deltaTime :: ReactG s Time
deltaTime = pick <$> exper (S.singleton $ DeltaTime Request)
	where
	pick evs = case S.elems $ S.filter (== DeltaTime Request) evs of
		[DeltaTime (Occurred t)] -> t
		_ -> error "never occur"

tryWait :: Time -> ReactG s Time
tryWait t0 = pick <$> exper (S.singleton $ TryWait t0 Request)
	where
	pick evs = case S.elems $ S.filter (== TryWait t0 Request) evs of
		[TryWait _ (Occurred t)] -> t
		es -> error $ "never occur: " ++ show es

sleep :: Time -> ReactG s ()
sleep t = do
	t' <- tryWait t
	if t' == t then pure () else sleep (t - t')
