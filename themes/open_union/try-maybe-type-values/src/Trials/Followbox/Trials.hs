{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Followbox.Trials where

import Prelude hiding (log)

import Control.Monad.State
import System.Environment

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import MonadicFrp
import Trials.Followbox
import Trials.Followbox.Event
import Trials.Followbox.Handlers

import Trials.Followbox.TestMonad

getGithubToken :: IO (Maybe (BS.ByteString, FilePath))
getGithubToken =
	(<$> getArgs) \case [nm, tkn] -> Just (BSC.pack nm, tkn); _ -> Nothing

tryHttpGet :: IO ()
tryHttpGet = getGithubToken >>= \mba ->
	interpret (handleHttpGet mba) (httpGet "https://api.github.com/users") >>= print

tryHttpGetTest :: TestMonad ()
tryHttpGetTest = interpret testHandleHttpGet (httpGet "https://api.github.com/users") >>= log . show

tryGetUsersJson :: IO ()
tryGetUsersJson = getGithubToken >>= \mba ->
	interpret (handleHttpGet mba) ((take 3 <$>) <$> getUsersJson) >>= either putStrLn (print `mapM_`)

tryGetUser1 :: IO ()
tryGetUser1 = getGithubToken >>= \mba ->
	interpret (handle mba) getUser1 `runStateT` [] >>= print . fst

tryGetUser1Test :: TestMonad ()
tryGetUser1Test = interpret testHandle getUser1UntilError >>= log . show

tryGetUser3 :: IO ()
tryGetUser3 = getGithubToken >>= \mba ->
	interpret (handle mba) (getUserN 3) `runStateT` [] >>= print . fst

tryGetLoginName3 :: IO ()
tryGetLoginName3 = getGithubToken >>= \mba ->
	interpret (handle mba) (getLoginNameNUntilError 3) `runStateT` [] >>= print . fst

tryGetLoginName3Test :: TestMonad ()
tryGetLoginName3Test = interpret testHandle (getLoginNameNUntilError 3) >>= log . show

tryLeftClickUser3 :: IO ()
tryLeftClickUser3 = getGithubToken >>= \mba ->
	interpret (handle mba) (leftClickUserN 3) `runStateT` [] >>= print . fst
