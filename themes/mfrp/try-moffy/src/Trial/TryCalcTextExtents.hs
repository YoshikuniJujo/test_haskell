{-# LANGUAGE TupleSections, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.TryCalcTextExtents where

import Control.Moffy
import Control.Moffy.Event.Delete
import Control.Moffy.Event.CalcTextExtents
import Control.Moffy.Handle
import Control.Moffy.Handle.Time
import Control.Moffy.Handle.XField
import Control.Moffy.Handle.GtkField
import Control.Moffy.Run
import Control.Concurrent.STM hiding (retry)
import Data.Type.Set
import Data.Time.Clock.System
import Graphics.Gtk hiding (DeleteEvent)

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

runTryCalcTextExtentsGtk :: IO ()
runTryCalcTextExtentsGtk = do
	(cr, c, c' :: TChan [()]) <- tryUseTChan
--	fst <$> ((interpretSt (handleBoxesFoo 0.1 cr c) (atomically . writeTChan c') (tryCalcTextExtents "hello") . (InitialMode ,) . systemToTAITime =<< getSystemTime)
	fst <$> ((interpretSt (handleBoxesFoo 0.1 cr c) print (tryCalcTextExtents "hello") . (InitialMode ,) . systemToTAITime =<< getSystemTime)
		<* gtkMainQuit)

instance Drawable () where
	draw _ _ = pure ()
