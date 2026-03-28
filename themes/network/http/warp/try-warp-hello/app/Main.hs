{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Data.ByteString.Lazy.Char8 qualified as LBSC
import Data.Time
import Control.Exception
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types

main :: IO ()
main = runEnv 10000 app

app :: Application
app req respond = bracket_
	(putStrLn "Allocating scarce resource")
	(putStrLn "Cleaning up") do
	now <- getCurrentTime
	respond . responseLBS status200 [] $
		"Hello, world! " <> LBSC.pack (show now)
