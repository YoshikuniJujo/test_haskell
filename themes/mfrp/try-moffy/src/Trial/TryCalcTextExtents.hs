{-# LANGUAGE BlockArguments, TupleSections, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.TryCalcTextExtents where

import Control.Monad
import Control.Moffy
import Control.Moffy.Event.Delete
import Control.Moffy.Event.Window
import Control.Moffy.Event.CalcTextExtents
import Control.Moffy.Handle
import Control.Moffy.Handle.Time
import Control.Moffy.Handle.XField as X
import Control.Moffy.Handle.TChan as T
import Control.Moffy.Run
import Control.Concurrent.STM hiding (retry)
import Data.Type.Set
import Data.Time.Clock.System
import Graphics.Gtk hiding (DeleteEvent)

import qualified Data.Text as T

import Field

import Data.Time
import Data.Time.Clock.TAI
import Control.Moffy.Event.Time

import Control.Moffy.Run.GtkField as G

tryCalcTextExtents :: T.Text -> Sig s (WindowNew :- DeleteEvent :- CalcTextExtents :- 'Nil) (T.Text, TextExtents') ()
tryCalcTextExtents txt = void do
	i <- waitFor $ adjust windowNew
	(emit . (txt ,) =<< waitFor (adjust $ calcTextExtents' "Sans" 30 txt)) >> waitFor (adjust $ deleteEvent i)

runTryCalcTextExtents :: IO ()
runTryCalcTextExtents = do
	f <- openField ("Hello, TextExtents!" :: String) []
	interpret (retry $ handle' Nothing f) print (tryCalcTextExtents "Hello, world!")
		<* closeField f

runTryCalcTextExtentsGtk :: IO ()
runTryCalcTextExtentsGtk = do
	([], (cr, c, _c' :: TChan [()])) <- runGtkMain (\_ _ _ -> pure ()) []
	fst <$> ((interpretSt (handleBoxesFoo 0.1 cr c) print (tryCalcTextExtents "Text, hello jj!") . (InitialMode ,) . systemToTAITime =<< getSystemTime)
		<* gtkMainQuit)

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f x y z = f (x, y, z)

handleBoxesFoo :: DiffTime -> TChan (EvReqs G.GuiEv) -> TChan (EvOccs G.GuiEv) ->
	HandleSt (Mode, AbsoluteTime) IO (TimeEv :+: G.GuiEv)
handleBoxesFoo dt cr co = retrySt
	$ ((\f x y z -> f (x, (y, z))) . popInput . handleTimeEvPlus . pushInput) (\(x, (y, z)) -> (((liftHandle' .) .) . T.handle . Just) x y z) dt cr co
