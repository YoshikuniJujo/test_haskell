{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module UseDebug where

import Data.Bool
import Debug

checkDebug :: String
checkDebug = bool "Not debug mode" "Debug mode" debug
