{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module BasicAuth (httpBasicAuth, getUsers) where

import Network.HTTP.Simple

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS

url :: Request
url = setRequestHeader "User-Agent" ["Yoshio"] "https://api.github.com/users"

removeLastNL :: BS.ByteString -> BS.ByteString
removeLastNL ba
	| Just (bs, '\n') <- BSC.unsnoc ba = bs
	| otherwise = ba

getUsers :: BS.ByteString -> FilePath -> IO (Response LBS.ByteString)
getUsers u fp = httpBasicAuth u fp url

httpBasicAuth :: BS.ByteString -> FilePath -> Request -> IO (Response LBS.ByteString)
httpBasicAuth usr fp rq = do
	p <- BS.readFile fp
	httpLBS $ setRequestBasicAuth usr (removeLastNL p) rq
