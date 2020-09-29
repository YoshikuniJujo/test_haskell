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

import qualified Data.Map as Map
import Data.Type.Flip

tryCalcTextExtents :: T.Text -> Sig s (WindowNew :- DeleteEvent :- CalcTextExtents :- 'Nil) (Map.Map WindowId (T.Text, TextExtents)) ()
tryCalcTextExtents txt = void do
	i <- waitFor $ adjust windowNew
	Map.singleton i <$%>
		((emit . (txt ,) =<< waitFor (adjust $ calcTextExtents i "Sans" 30 txt)) >> waitFor (adjust $ deleteEvent i))

runTryCalcTextExtents :: IO ()
runTryCalcTextExtents = do
	f <- openField ("Hello, TextExtents!" :: String) []
	interpret (retry $ handle' Nothing f) print (tryCalcTextExtents "Hello, world!")
		<* closeField f

runTryCalcTextExtentsGtk :: T.Text -> IO ()
runTryCalcTextExtentsGtk t = do
	([], (cr, c, _c' :: TChan (Map.Map WindowId [()]))) <- runGtkMain (\_ _ _ -> pure ()) []
	fst <$> ((interpretSt (handleBoxesFoo 0.1 cr c) print (tryCalcTextExtents t) . (InitialMode ,) . systemToTAITime =<< getSystemTime)
		<* gtkMainQuit)

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f x y z = f (x, y, z)

handleBoxesFoo :: DiffTime -> TChan (EvReqs G.GuiEv) -> TChan (EvOccs G.GuiEv) ->
	HandleSt (Mode, AbsoluteTime) IO (TimeEv :+: G.GuiEv)
handleBoxesFoo dt cr co = retrySt
	$ ((\f x y z -> f (x, (y, z))) . popInput . handleTimeEvPlus . pushInput) (\(x, (y, z)) -> (((liftHandle' .) .) . T.handle . Just) x y z) dt cr co
