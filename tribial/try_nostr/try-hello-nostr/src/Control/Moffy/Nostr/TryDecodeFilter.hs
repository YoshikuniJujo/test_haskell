{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Nostr.TryDecodeFilter where

import Control.Moffy
import Control.Moffy.Nostr.Event
import Control.Moffy.Nostr.Run
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Nostr.Event qualified as Event
import Nostr.Filter qualified as Filter
import Nostr.Filter.Json qualified as FltJsn

import Tools

sample :: IO ()
sample = run (const print) "nos.lol" "443" do
	waitFor $ request "req-1" FltJsn.null
	emit . snd =<< waitFor awaitEvent
	waitFor halt

authorFilter :: FilePath -> IO Filter.Filter
authorFilter fp = do
	Right pk <- Event.publicFromBech32 <$> T.readFile fp
	pure FltJsn.null {
		Filter.authors = Just [pk],
		Filter.kinds = Just [1], Filter.limit = Just 5 }

sampleAuthor :: FilePath -> FilePath -> IO ()
sampleAuthor scfp pbfp = do
	flt <- authorFilter pbfp
	Right sc <- Event.secretFromBech32 <$> T.readFile scfp
	run (const putStrLn) "nos.lol" "443" do
		waitFor $ request "req-1" flt
		ev <- waitFor (awaitNameEvent "req-1")
		emit . T.unpack $ Event.content ev
		emit . T.unpack . toHex $ Event.hash ev
		msg <- waitFor (awaitSignature sc ev)
		emit . show $ T.unpack . toHex <$> msg
		emit . show . (T.unpack . toHex <$>) =<< waitFor (awaitSignature sc ev)
		emit . show $ Event.verify ev <$> msg
		waitFor halt
