{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Followbox.Trials where

import Prelude hiding (log, until)

import Control.Monad.State
import System.Environment

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T

import MonadicFrp.Run
import MonadicFrp (until, first)
import Trials.Followbox
import Trials.Followbox.Event
import Trials.Followbox.Handlers
import Trials.Followbox.View

import Trials.Followbox.TestMonad

import Field

getGithubToken :: IO (Maybe (BS.ByteString, FilePath))
getGithubToken =
	(<$> getArgs) \case [nm, tkn] -> Just (BSC.pack nm, tkn); _ -> Nothing

tryHttpGet :: IO ()
tryHttpGet = getGithubToken >>= \mba ->
	interpretReact (handleHttpGet mba) (httpGet "https://api.github.com/users") >>= print

tryHttpGetTest :: TestMonad ()
tryHttpGetTest = interpretReact testHandleHttpGet (httpGet "https://api.github.com/users") >>= log . show

tryGetUsersJson :: IO ()
tryGetUsersJson = getGithubToken >>= \mba ->
	interpretReact (handleHttpGet mba) ((take 3 <$>) <$> getUsersJson) >>= either putStrLn (print `mapM_`)

tryGetUser1 :: IO ()
tryGetUser1 = getGithubToken >>= \mba ->
	interpretReact (handle mba) getUser1 `runStateT` [] >>= print . fst

tryGetUser1Test :: TestMonad ()
tryGetUser1Test = interpretReact testHandle getUser1UntilError >>= log . show

tryGetUser3 :: IO ()
tryGetUser3 = getGithubToken >>= \mba ->
	interpretReact (handle mba) (getUserN 3) `runStateT` [] >>= print . fst

tryLeftClickUser3 :: IO ()
tryLeftClickUser3 = getGithubToken >>= \mba ->
	interpretReact (handle mba) (leftClickUserN 3) `runStateT` [] >>= print . fst

tryGetLoginNameQuit' :: IO ()
tryGetLoginNameQuit' = getGithubToken >>= \mba -> do
	f <- openField ("tryGetLoginNameQuit'" :: String) [exposureMask, buttonPressMask]
	interpret (handle' f mba) (liftIO . view f) getLoginNameQuit `runStateT` [] >>= print . fst
	closeField f

tryGetLoginNameNQuit :: IO ()
tryGetLoginNameNQuit = getGithubToken >>= \mba -> do
	f <- openField ("tryGetLoginNameNQuit'" :: String) [exposureMask, buttonPressMask]
	() <$ interpret (handle' f mba) (liftIO . view f) getLoginNameNQuit `runStateT` []
	closeField f

tryCalcTextExtents :: IO ()
tryCalcTextExtents = do
	f <- openField ("tryCalcTextExtents" :: String) [exposureMask]
	let	greeting = "Hello, world!" :: T.Text
	interpretReact (handleCalcTextExtents f) (calcTextExtents "sans" 12.5 greeting) >>= print
	closeField f

tryMousePosition :: IO ()
tryMousePosition = do
	f <- openField ("tryMousePosition" :: String) [exposureMask, pointerMotionMask, buttonPressMask]
	interpret (handle' f Nothing) (liftIO . print) (mousePosition `until` checkQuit) `runStateT` [] >>= print . fst
	closeField f

tryViewLoginNameSig :: IO ()
tryViewLoginNameSig = getGithubToken >>= \mba -> do
	f <- openField ("tryViewLoginNameSig" :: String) [exposureMask, buttonPressMask]
	() <$ interpret (handle' f mba) (liftIO . view f) (viewLoginNameSig 0 "foo" `until` checkQuit) `runStateT` []
	closeField f

tryViewMultiLoginNameSig :: IO ()
tryViewMultiLoginNameSig = getGithubToken >>= \mba -> do
	f <- openField ("tryViewMultiLoginNameSig" :: String) [exposureMask, buttonPressMask]
	() <$ interpret (handle' f mba) (liftIO . view f) (viewMultiLoginNameSig 3 `until` checkQuit) `runStateT` []
	closeField f

tryGetAvatarAddress :: IO ()
tryGetAvatarAddress = getGithubToken >>= \mba -> do
	f <- openField ("tryGetAvatarAddress" :: String) [exposureMask]
	interpretReact (handle' f mba) (getAvatarAddress `first` catchError) `runStateT` [] >>= print . fst
	closeField f

tryViewAvatar :: IO ()
tryViewAvatar = getGithubToken >>= \mba -> do
	f <- openField ("tryViewAvatar" :: String) [exposureMask, buttonPressMask]
	() <$ interpret (handle' f mba) (liftIO . view f) (viewAvatar `until` catchError) `runStateT` []
	closeField f
