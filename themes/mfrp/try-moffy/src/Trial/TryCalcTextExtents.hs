{-# LANGUAGE TupleSections, OverloadedStrings #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.TryCalcTextExtents where

import Control.Moffy
import Control.Moffy.Event.Delete
import Control.Moffy.Event.CalcTextExtents
import Control.Moffy.Handle
import Control.Moffy.Handle.XField
import Control.Moffy.Run
import Data.Type.Set

import qualified Data.Text as T

import Field

tryCalcTextExtents :: T.Text -> Sig s (DeleteEvent :- CalcTextExtents :- 'Nil) (T.Text, TextExtents) ()
tryCalcTextExtents txt =
	(emit . (txt ,) =<< waitFor (adjust $ calcTextExtents "Sans" 20 txt)) >> waitFor (adjust deleteEvent)

runTryCalcTextExtents :: IO ()
runTryCalcTextExtents = do
	f <- openField ("Hello, TextExtents!" :: String) []
	interpret (retry $ handle' Nothing f) print (tryCalcTextExtents "Hello, world!")
		<* closeField f
