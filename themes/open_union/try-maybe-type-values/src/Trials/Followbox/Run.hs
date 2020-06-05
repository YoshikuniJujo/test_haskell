{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Followbox.Run (runFollowbox) where

import Control.Monad.State (runStateT, lift)
import Data.List (sort)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Console.GetOpt (ArgOrder(..), OptDescr(..), ArgDescr(..), getOpt)
import System.Random (mkStdGen)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import MonadicFrp.Run (interpret)
import Trials.Followbox.Event (SigF)
import Trials.Followbox.Handle (
	FollowboxState, handleFollowbox, initialFollowboxState )
import Trials.Followbox.View (View, view)
import Trials.Followbox.TypeSynonym (
	WindowTitle, Browser, GithubNameToken, GithubUserName )
import Field (openField, closeField, exposureMask, buttonPressMask)

---------------------------------------------------------------------------

-- * RUN FOLLOWBOX
-- * GET FOLLOWBOX INFO
-- * GET OPT

---------------------------------------------------------------------------
-- RUN FOLLOWBOX
---------------------------------------------------------------------------

defaultBrowser :: Browser
defaultBrowser = "firefox"

runFollowbox :: WindowTitle -> SigF View a -> IO (a, FollowboxState)
runFollowbox ttl sig = getFollowboxInfo >>= \case
	Left em -> putStrLn em >> exitFailure
	Right fi -> runFb ttl fi sig

runFb :: WindowTitle -> FollowboxInfo -> SigF View a -> IO (a, FollowboxState)
runFb ttl i sg = do
	f <- openField ttl [exposureMask, buttonPressMask]
	interpret (handleFollowbox f brs gnt) (lift . view f) sg
		`runStateT` initialFollowboxState (mkStdGen 8) <* closeField f
	where FollowboxInfo { fiBrowser = brs, fiGithubUserNameToken = gnt } = i

---------------------------------------------------------------------------
-- GET FOLLOWBOX INFO
---------------------------------------------------------------------------

getFollowboxInfo :: IO (Either String FollowboxInfo)
getFollowboxInfo = do
	(os, args, ems) <- getOpt Permute followboxOptions <$> getArgs
	case (args, ems) of
		([], []) ->
			either (pure . Left) optionsToInfo . chkDupOpt $ sort os
		(_, []) -> pure $ Left "Only options are permitted"
		_ -> pure . Left $ concat ems

data FollowboxInfo = FollowboxInfo {
	fiBrowser :: Browser,
	fiGithubUserNameToken :: Maybe GithubNameToken }
	deriving Show

optionsToInfo :: [FollowboxOption] -> IO (Either String FollowboxInfo)
optionsToInfo [] = pure $ Right FollowboxInfo {
	fiBrowser = defaultBrowser, fiGithubUserNameToken = Nothing }
optionsToInfo (FoBrowser brs : os) =
	((\fi -> fi { fiBrowser = brs }) <$>) <$> optionsToInfo os
optionsToInfo (FoGithubUserName un : FoGithubToken fp : os) = do
	r <- optionsToInfo os
	tkn <- trim <$> BS.readFile fp
	pure $ (\fi -> fi { fiGithubUserNameToken = Just (un, tkn) }) <$> r
	where trim ba | Just (bs, '\n') <- BSC.unsnoc ba = bs | otherwise = ba
optionsToInfo _ = pure $ Left "Bad option set"

---------------------------------------------------------------------------
-- GET OPT
---------------------------------------------------------------------------

data FollowboxOption
	= FoBrowser Browser
	| FoGithubUserName GithubUserName | FoGithubToken FilePath
	deriving (Show, Eq, Ord)

chkDupOpt :: [FollowboxOption] -> Either String [FollowboxOption]
chkDupOpt [] = Right []
chkDupOpt (FoBrowser _ : FoBrowser _ : _) =
	Left "Duplicate Browser options"
chkDupOpt (FoGithubUserName _ : FoGithubUserName _ : _) =
	Left "Duplicate GitHub user options"
chkDupOpt (FoGithubToken _ : FoGithubToken _ : _) =
	Left "Duplicate GitHub token options"
chkDupOpt (o : os) = (o :) <$> chkDupOpt os

followboxOptions :: [OptDescr FollowboxOption]
followboxOptions = [
	Option "b" ["browser"] (ReqArg FoBrowser "browser")
		"Set browser to access GitHub user page",
	Option "u" ["github-user-name"]
		(ReqArg (FoGithubUserName . BSC.pack) "GitHub user name")
		"Set GitHub user name",
	Option "t" ["github-token"]
		(ReqArg FoGithubToken "GitHub token") "Set GitHub token" ]
