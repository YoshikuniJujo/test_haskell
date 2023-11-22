{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Prelude hiding (break)

import Control.Monad
import Control.Moffy
import Control.Moffy.Event.Delete
import Control.Moffy.Event.Window
import Control.Moffy.Event.DefaultWindow
import Control.Moffy.Event.Mouse
import Trial.Draw.Viewable
import Trial.TryScroll

import Data.Type.Set
import Data.Type.Flip

import qualified Data.Map as Map

main :: IO ()
main = void $ runTryScroll (\_ _ -> mapM_ putMessage) do
	i <- waitFor $ adjust windowNew
	waitFor . adjust $ storeDefaultWindow i
	Map.singleton i <$%>
		(void . adjustSig $ tryScroll `break` deleteEvent i :: Sig s (WindowNew :- DefaultWindowEv :+: DeleteEvent :- MouseScroll :- 'Nil) [Message] ())
