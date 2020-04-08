{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Followbox.BasicAuth (httpBasicAuth) where

import Network.HTTP.Simple

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS

removeLastNL :: BS.ByteString -> BS.ByteString
removeLastNL ba
	| Just (bs, '\n') <- BSC.unsnoc ba = bs
	| otherwise = ba

httpBasicAuth :: BS.ByteString -> FilePath -> Request -> IO (Response LBS.ByteString)
httpBasicAuth usr fp rq = do
	p <- BS.readFile fp
	httpLBS $ setRequestBasicAuth usr (removeLastNL p) rq
