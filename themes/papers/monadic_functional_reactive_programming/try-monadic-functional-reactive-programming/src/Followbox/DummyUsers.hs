{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Followbox.DummyUsers where

import GHC.Generics
import Data.Aeson

import qualified Data.ByteString.Lazy as LBS

data User = User {
	login :: String,
	avatar_url :: String,
	html_url :: String }
	deriving (Generic, Show)

instance ToJSON User
instance FromJSON User

dummyUsers :: Int -> LBS.ByteString
dummyUsers n = encode $ (<$> [1 :: Int .. 10]) \i -> User {
	login = "user_" ++ show n ++ "-" ++ show i,
	avatar_url = "https://avatarsN.githubusercontent.com/foo/bar?v=123",
	html_url = "https://github.com/YoshikuniJujo" }
