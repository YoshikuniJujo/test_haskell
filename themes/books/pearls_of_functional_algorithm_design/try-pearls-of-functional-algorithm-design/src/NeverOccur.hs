{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module NeverOccur (NeverOccur, neverOccur, neverNothing) where

import GHC.Stack (CallStack, HasCallStack, callStack, prettyCallStack)
import Control.Exception (Exception, throw)
import Data.Maybe (fromMaybe)

data NeverOccur = NeverOccur CallStack
instance Exception NeverOccur
instance Show NeverOccur where
	show (NeverOccur s) = "NeverOccur\n" ++ prettyCallStack s

neverOccur :: HasCallStack => a
neverOccur = throw $ NeverOccur callStack

neverNothing :: HasCallStack => Maybe a -> a
neverNothing = fromMaybe neverOccur
