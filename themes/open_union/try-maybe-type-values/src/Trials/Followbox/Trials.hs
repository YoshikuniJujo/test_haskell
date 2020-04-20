{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Followbox.Trials where

import Prelude hiding (log, until)

import Control.Monad.State
import Data.Time
import System.Environment
import System.Random

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import MonadicFrp.Run
import MonadicFrp (until)
import Trials.Followbox
import Trials.Followbox.Event
import Trials.Followbox.Handle
import Trials.Followbox.View
import Trials.Followbox.Wrapper.Aeson

import Field

browser :: FilePath
browser = "firefox"

initialState :: (StdGen, [Object], Maybe UTCTime)
initialState = (mkStdGen 8, [], Nothing)

getGithubToken :: IO (Maybe (BS.ByteString, FilePath))
getGithubToken =
	(<$> getArgs) \case [nm, tkn] -> Just (BSC.pack nm, tkn); _ -> Nothing

tryGetUser1 :: IO ()
tryGetUser1 = getGithubToken >>= \mba -> do
	f <- openField ("tryGetUser1" :: String) [exposureMask, buttonPressMask]
	interpretReact (handle f browser mba) getUser1 `runStateT` initialState >>= print . fst
	closeField f

tryGetUser3 :: IO ()
tryGetUser3 = getGithubToken >>= \mba -> do
	f <- openField ("tryGetUser3" :: String) [exposureMask, buttonPressMask]
	interpretReact (handle f browser mba) (getUserN 3) `runStateT` initialState >>= print . fst
	closeField f

tryLeftClickUser3 :: IO ()
tryLeftClickUser3 = getGithubToken >>= \mba -> do
	f <- openField ("tryLeftClickUser3" :: String) [exposureMask, buttonPressMask]
	interpretReact (handle f browser mba) (leftClickUserN 3) `runStateT` initialState >>= print . fst
	closeField f

tryGetLoginNameQuit' :: IO ()
tryGetLoginNameQuit' = getGithubToken >>= \mba -> do
	f <- openField ("tryGetLoginNameQuit'" :: String) [exposureMask, buttonPressMask]
	interpret (handle f browser mba) (liftIO . view f) getLoginNameQuit `runStateT` initialState >>= print . fst
	closeField f

tryGetLoginNameNQuit :: IO ()
tryGetLoginNameNQuit = getGithubToken >>= \mba -> do
	f <- openField ("tryGetLoginNameNQuit'" :: String) [exposureMask, buttonPressMask]
	() <$ interpret (handle f browser mba) (liftIO . view f) getLoginNameNQuit `runStateT` initialState
	closeField f

tryViewMultiLoginName :: IO ()
tryViewMultiLoginName = getGithubToken >>= \mba -> do
	f <- openField ("tryViewMultiLoginName" :: String) [exposureMask, buttonPressMask]
	() <$ interpret (handle f browser mba) (liftIO . view f) (viewMultiLoginName 3 `until` checkQuit `until` terminateOccur) `runStateT` initialState
	closeField f
