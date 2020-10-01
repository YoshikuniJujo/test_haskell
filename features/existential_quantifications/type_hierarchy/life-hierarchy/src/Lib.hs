{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Control.Exception
import Control.Exception.Hierarchy
import Data.Typeable

data BarException = BarException deriving (Typeable, Show)

exceptionHierarchy Nothing $ ExNode "FooException" [ExType ''BarException]
