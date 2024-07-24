{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Relex (

-- * API

setRelex, relex, withRelex, StartCode, RelexAction,


-- * CLASS

RelexMonad(..)

) where

type StartCode = Int
type RelexAction m t = RelexInput m -> Int -> m t

setRelex :: forall m t . RelexMonad m => StartCode -> m t -> RelexAction m t
setRelex scd skp i ln = do
	setRelexPosn $ toRelexPosn @m i
	pushRelexWord . take ln $ toRelexSource @m i
	setRelexStartCode scd
	skp

relex :: forall m t . RelexMonad m =>
	(String -> String) -> StartCode -> m t -> RelexAction m t
relex cnv scd skp i ln = do
	ps <- getRelexPosn
	s <- cnv <$> readRelexWords (take ln $ toRelexSource @m i)
	clearRelexWords
	setRelexInput $ setPosnAndModifySource @m ps ((s ++) . drop ln) i
	setRelexStartCode scd
	skp

withRelex :: forall m t . RelexMonad m => (String -> t) -> RelexAction m t
withRelex tkn i ln = tkn w <$ pushRelexWord w
	where w = take ln $ toRelexSource @m i

setPosnAndModifySource :: forall m . RelexMonad m =>
	RelexPosn m -> (String -> String) -> RelexInput m -> RelexInput m
setPosnAndModifySource ps cnv i = let
	osrc = toRelexSource @m i in
	fromRelexPosn @m ps $ fromRelexSource @m (cnv osrc) i

class Monad m => RelexMonad m where
	type RelexInput m
	type RelexPosn m

	toRelexPosn :: RelexInput m -> RelexPosn m
	fromRelexPosn :: RelexPosn m -> RelexInput m -> RelexInput m
	toRelexSource :: RelexInput m -> String
	fromRelexSource :: String -> RelexInput m -> RelexInput m

	setRelexPosn :: RelexPosn m -> m ()
	getRelexPosn :: m (RelexPosn m)
	pushRelexWord :: String -> m ()
	readRelexWords :: String -> m String
	clearRelexWords :: m ()
	setRelexInput :: RelexInput m -> m ()
	setRelexStartCode :: StartCode -> m ()
