module Main where

import Prelude hiding (break)

import Control.Monad
import Control.Moffy
import Control.Moffy.Event.Delete
import Trial.Draw.Viewable
import Trial.TryScroll

main :: IO ()
main = void . runTryScroll (\_ _ -> mapM_ putMessage) $ tryScroll `break` deleteEvent
