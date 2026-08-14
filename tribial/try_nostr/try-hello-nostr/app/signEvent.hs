{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.ByteString.Lazy qualified as BSL
import Data.Text.IO qualified as T
import System.Environment
import Data.Aeson qualified as A

import Nostr.Event qualified as Event
import Nostr.Event.Json qualified as EvJs
import AddSignature

main :: IO ()
main = do
	skf : pkf : evf : _ <- getArgs
	Right sk <- Event.secretFromBech32 <$> T.readFile skf
	Right pk <- Event.publicFromBech32 <$> T.readFile pkf
	Just ev <-  (EvJs.decodeNoPub =<<) . A.decode <$> BSL.readFile evf
	print . EvJs.encode' =<< signature sk pk ev
	print . EvJs.encode' =<< maybeToIO (signature' sk pk ev)

maybeToIO :: Maybe a -> IO a
maybeToIO mx = do
	Just x <- pure mx
	pure x
