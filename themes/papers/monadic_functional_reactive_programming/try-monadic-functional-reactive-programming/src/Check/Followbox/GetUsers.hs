{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Check.Followbox.GetUsers (getUsers, decodeUsers) where

import Network.HTTP.Simple
import Data.Aeson

import qualified Data.ByteString.Lazy as LBS

getUsers :: IO LBS.ByteString
getUsers = getResponseBody <$> httpLBS (setRequestHeader "User-Agent" ["Yoshio"] "https://api.github.com/users")

decodeUsers :: LBS.ByteString -> Either String [Object]
decodeUsers = eitherDecode
