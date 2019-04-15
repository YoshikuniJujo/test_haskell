{-# LANGUAGE DataKinds, TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Prelude hiding (head)
import Control.Monad.IO.Class
import Network.Wai.Handler.Warp
import Text.Blaze.Html5 hiding (main)
import Servant
import Servant.HTML.Blaze

data HelloWorld = HelloWorld deriving Show

instance ToMarkup HelloWorld where
	toMarkup _ = docTypeHtml $ do
		head . title $ toHtml "Say Hello"
		body $ toHtml "Hello, world!"

type Api = Get '[HTML] HelloWorld

server :: Server Api
server = return HelloWorld

main :: IO ()
main = run 8080 $ serve @Api Proxy server
