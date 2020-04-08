{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Followbox.Trials where

import System.Environment

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import MonadicFrp
import Trials.Followbox
import Trials.Followbox.Event
import Trials.Followbox.Handlers

getGithubToken :: IO (Maybe (BS.ByteString, FilePath))
getGithubToken =
	(<$> getArgs) \case [nm, tkn] -> Just (BSC.pack nm, tkn); _ -> Nothing

tryHttpGet :: IO ()
tryHttpGet = getGithubToken >>= \mba ->
	interpret (handle mba) (httpGet "https://api.github.com/users") >>= print

tryGetUsersJson :: IO ()
tryGetUsersJson = getGithubToken >>= \mba ->
	interpret (handle mba) ((take 3 <$>) <$> getUsersJson) >>= either putStrLn (print `mapM_`)
