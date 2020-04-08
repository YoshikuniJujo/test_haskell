{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Followbox.Trials where

import MonadicFrp
import Trials.Followbox.Event
import Trials.Followbox.Handlers

tryHttpGet :: IO ()
tryHttpGet = interpret handle (httpGet "https://api.github.com/users") >>= print
