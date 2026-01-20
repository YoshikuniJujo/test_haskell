{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Data.Text
import Wuss
import Network.WebSockets

main :: IO ()
main = runSecureClient "nos.lol" 443 "/" ws

ws :: ClientApp ()
ws cnn = do
	putStrLn "Connected!"

	sendTextData cnn ("[\"REQ\", \"foobar12345\", { \"kinds\": [1] }]" :: Text)

	print @Text =<< receiveData cnn

	sendClose cnn ("Bye!" :: Text)
