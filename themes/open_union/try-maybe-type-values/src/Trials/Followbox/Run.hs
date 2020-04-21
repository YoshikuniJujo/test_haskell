{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Followbox.Run (runFollowbox, Browser, WindowTitle) where

import Control.Monad.State
import Data.Time
import System.Environment
import System.Random

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import MonadicFrp.Run
import Trials.Followbox.Event
import Trials.Followbox.Handle
import Trials.Followbox.View
import Trials.Followbox.Wrapper.Aeson

import Field

type Browser = FilePath
type WindowTitle = String

initialState :: (StdGen, [Object], Maybe UTCTime)
initialState = (mkStdGen 8, [], Nothing)

getGithubToken :: IO (Maybe (BS.ByteString, FilePath))
getGithubToken =
	(<$> getArgs) \case [nm, tkn] -> Just (BSC.pack nm, tkn); _ -> Nothing

runFollowbox :: Browser -> WindowTitle -> SigF View a -> IO (a, (StdGen, [Object], Maybe UTCTime))
runFollowbox brs ttl sg = getGithubToken >>= \mba -> do
	f <- openField ttl [exposureMask, buttonPressMask]
	interpret (handle f brs mba) (liftIO . view f) sg `runStateT` initialState <* closeField f
