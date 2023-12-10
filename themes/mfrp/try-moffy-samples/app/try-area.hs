{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Concurrent.STM (atomically, newTVar)
import Control.Moffy
import Control.Moffy.Handle
import Control.Moffy.Run
import Data.Type.Set
import Data.Map qualified as M

import Control.Moffy.Samples.Event.Area
import Control.Moffy.Samples.Handle.Area

main :: IO ()
main = print =<< do
	va <- atomically $ newTVar M.empty
	interpretReact (retry $ handle va) foo

foo :: React s (SetArea :- Singleton GetArea) (Point, Point)
foo = adjust (setArea 0 ((80, 50), (160, 100))) >> adjust (getArea 0)
