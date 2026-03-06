{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Nostr.TryDecodeFilter where

import Control.Moffy
import Control.Moffy.Nostr.Event
import Control.Moffy.Nostr.Run
import Nostr.Filter.Json qualified as FltJsn

sample :: IO ()
sample = run (const print) "nos.lol" "443" do
	waitFor $ request "req-1" FltJsn.null
	emit . snd =<< waitFor awaitEvent
	waitFor halt
