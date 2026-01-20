{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Data.Foldable
import Data.Text
import Data.Aeson
import Wuss
import Network.WebSockets

main :: IO ()
main = runSecureClient "nos.lol" 443 "/" ws

ws :: ClientApp ()
ws cnn = do
	putStrLn "Connected!"

	sendTextData cnn ("[\"REQ\", \"foobar12345\", { \"kinds\": [1] }]" :: Text)

	(Just (Array (toList -> [_, _, obj])) :: Maybe Value) <- decode <$> receiveData cnn

	print obj

	sendClose cnn ("Bye!" :: Text)
