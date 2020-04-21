{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Followbox.Trials (
	tryGetLoginNameQuit, tryGetLoginNameNQuit, tryViewMultiLoginName ) where

import Prelude hiding (log, until)

import MonadicFrp (until)

import Trials.Followbox
import Trials.Followbox.Run
import Trials.Followbox.Event

browser :: Browser
browser = "firefox"

tryGetLoginNameQuit :: IO ()
tryGetLoginNameQuit = () <$ runFollowbox browser "tryGetLoginNameQuit" getLoginNameQuit

tryGetLoginNameNQuit :: IO ()
tryGetLoginNameNQuit = () <$ runFollowbox browser "tryGetLoginNameNQuit" getLoginNameNQuit

tryViewMultiLoginName :: IO ()
tryViewMultiLoginName = () <$ runFollowbox browser "tryViewMultiLoginName"
	(viewMultiLoginName 3 `until` checkQuit `until` terminateOccur)
