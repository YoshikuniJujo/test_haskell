{-# LANGUAGE DataKinds, TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Prelude hiding (head)
import Control.Monad.IO.Class
import System.Environment
import System.FilePath
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import Text.Blaze.Html5 hiding (main)
import Servant
import Servant.HTML.Blaze

curubaCertFile, curubaIntFile, curubaKeyFile ::
	String -> String -> FilePath
curubaCertFile d n = d </> n <.> "cert"
curubaIntFile d n = d </> n ++ "_int.cert"
curubaKeyFile d n = d </> n <.> "key"

data HelloWorld = HelloWorld deriving Show

instance ToMarkup HelloWorld where
	toMarkup _ = docTypeHtml $ do
		head . title $ toHtml "Say Hello"
		body $ toHtml "Hello, world!"

type Api = Get '[HTML] HelloWorld

server :: Server Api
server = return HelloWorld

main :: IO ()
main = do
	crtd : crtn : _ <- getArgs
	let	ccf = curubaCertFile crtd crtn
		cif = curubaIntFile crtd crtn
		cky = curubaKeyFile crtd crtn
	runTLS	(tlsSettingsChain ccf [cif] cky)
		(setPort 8080 defaultSettings)
		$ serve @Api Proxy server
