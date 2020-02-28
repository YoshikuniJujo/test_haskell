{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Check.Followbox.GetUsers (getUsers) where

import Network.HTTP.Simple

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as A

import AesonObject

getUsers :: IO LBS.ByteString
getUsers = getResponseBody <$> httpLBS (setRequestHeader "User-Agent" ["Yoshio"] "https://api.github.com/users")
