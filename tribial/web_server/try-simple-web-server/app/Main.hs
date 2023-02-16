{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import System.Environment
import Data.String
import Data.Text qualified as T
import Data.Text.IO qualified as T

import Network.Wai.Handler.Warp as Warp
import Network.Wai as Wai
import Network.HTTP.Types as HTypes

main :: IO ()
main = do
	[prtn, fp] <- getArgs
	cnt <- readFile fp
	Warp.run 8000 . helloApp $ fromString cnt

-- helloApp :: T.Text -> Wai.Application
helloApp cnt req send = send $ Wai.responseBuilder HTypes.status200 [] cnt
