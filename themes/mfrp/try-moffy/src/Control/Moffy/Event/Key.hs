{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Event.Key (
	pattern AsciiKey,
	module Control.Moffy.Event.Key.XK
	) where

import Control.Moffy.Event.Key.XK
import Control.Moffy.Event.Key.Internal
import Data.Char

pattern AsciiKey :: Char -> Key
pattern AsciiKey c <- (toAsciiChar -> Just c)

toAsciiChar :: Key -> Maybe Char
toAsciiChar (Key k)
	| 0x20 <= k && k <= 0x7e = Just . chr $ fromIntegral k
	| otherwise = Nothing
