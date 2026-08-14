{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Data.Aeson qualified as A
import Data.UnixTime
import System.Environment

import Nostr.Event.NoPub qualified as NoPub
import Nostr.Event.Json qualified as EvJsn

main :: IO ()
main = do
	fp : cnt : _ <- getArgs
	ut <- getUnixTime
	BSL.writeFile fp . A.encode . EvJsn.encodeNoPub $ NoPub.E {
		NoPub.created_at = ut,
		NoPub.kind = 1,
		NoPub.tags = [],
		NoPub.content = T.pack cnt }
