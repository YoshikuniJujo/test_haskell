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
	(_, Just (fromIntegral -> s)) -> Just . Singleton . OccKeyDown $ Key s
	_ -> Nothing
