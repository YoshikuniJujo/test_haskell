{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad
import Data.Vector qualified as V
import Data.ByteString.Lazy.Char8 qualified as BSLC
import Data.Text qualified as T
import Data.Aeson qualified as A
import System.Environment
import Network.WebSockets
import Wuss

import Nostr.Filter
import Nostr.Filter.Json qualified as FlJsn

main :: IO ()
main = do
	scr : raddr : rprt : _ <- getArgs
	if (scr == "secure")
	then runSecureClient raddr (read rprt) "/" ws
	else runClient raddr (read rprt) "/" ws

ws :: ClientApp ()
ws cnn = do
	let	req0 = req "foobar12345" FlJsn.null
	sendTextData cnn req0
	forever $ print @T.Text =<< receiveData cnn

req :: T.Text -> Filter -> BSLC.ByteString
req nm = A.encode . A.Array . V.fromList
	. ([A.String "REQ", A.String nm] ++) . (: []) . FlJsn.encode
