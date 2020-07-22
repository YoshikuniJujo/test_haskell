{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Handle.XField.Key (pattern KeyEv) where

import Control.Moffy
import Control.Moffy.Event.Key
import Data.OneOrMore
import Field

pattern KeyEv :: EvOccs KeyEv -> Event'
pattern KeyEv ev <- (keyEv -> Just ev)

keyEv :: Event' -> Maybe (EvOccs KeyEv)
keyEv = \case
	(KeyEvent { ev_event_type = 2 }, Just (fromIntegral -> s)) -> Just . expand . Singleton . OccKeyDown $ Key s
	(KeyEvent { ev_event_type = 3 }, Just (fromIntegral -> s)) -> Just . expand . Singleton . OccKeyUp $ Key s
	_ -> Nothing
