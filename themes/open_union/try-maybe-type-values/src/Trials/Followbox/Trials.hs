{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Followbox.Trials where

import Control.Monad.State
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
	interpret (handleHttpGet mba) (httpGet "https://api.github.com/users") >>= print

tryGetUsersJson :: IO ()
tryGetUsersJson = getGithubToken >>= \mba ->
	interpret (handleHttpGet mba) ((take 3 <$>) <$> getUsersJson) >>= either putStrLn (print `mapM_`)

tryGetUser1 :: IO ()
tryGetUser1 = getGithubToken >>= \mba ->
	interpret (handle mba) getUser1 `runStateT` [] >>= print . fst

tryGetUser3 :: IO ()
tryGetUser3 = getGithubToken >>= \mba ->
	interpret (handle mba) (getUserN 3) `runStateT` [] >>= print . fst

tryLeftClickUser3 :: IO ()
tryLeftClickUser3 = getGithubToken >>= \mba ->
	interpret (handle mba) (leftClickUserN 3) `runStateT` [] >>= print . fst
