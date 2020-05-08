{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Followbox.Run (runFollowbox) where

import Control.Monad.State (runStateT, lift)
import Data.List (sort)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Console.GetOpt (ArgOrder(..), OptDescr(..), ArgDescr(..), getOpt)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import MonadicFrp.Run (interpret)
import Trials.Followbox.Event (SigF)
import Trials.Followbox.Handle (handle, FollowboxState, initialFollowboxState)
import Trials.Followbox.View (View, view)
import Trials.Followbox.TypeSynonym (
	WindowTitle, Browser, GithubNameToken, GithubUserName )
import Field (openField, closeField, exposureMask, buttonPressMask)

---------------------------------------------------------------------------

defaultBrowser :: Browser
defaultBrowser = "firefox"

initialState :: FollowboxState
initialState = initialFollowboxState

runFollowbox :: WindowTitle -> SigF View a -> IO (a, FollowboxState)
runFollowbox ttl sig = getFollowboxInfo >>= \case
	Left em -> putStrLn em >> exitFailure
	Right fi -> runFollowboxGen ttl fi sig

runFollowboxGen :: WindowTitle -> FollowboxInfo -> SigF View a -> IO (a, FollowboxState)
runFollowboxGen ttl fi sg = do
	f <- openField ttl [exposureMask, buttonPressMask]
	interpret (handle f brs mba) (lift . view f) sg `runStateT` initialState <* closeField f
	where FollowboxInfo { fiBrowser = brs, fiGithubUserNameToken = mba } = fi

data FollowboxInfo = FollowboxInfo {
	fiBrowser :: Browser,
	fiGithubUserNameToken :: Maybe GithubNameToken }
	deriving Show

data FollowboxOption
	= FoBrowser Browser
	| FoGithubUserName GithubUserName | FoGithubToken FilePath
	deriving (Show, Eq, Ord)

checkDupOption :: [FollowboxOption] -> Either String [FollowboxOption]
checkDupOption [] = Right []
checkDupOption (FoBrowser _ : FoBrowser _ : _) =
	Left "Duplicate Browser options"
checkDupOption (FoGithubUserName _ : FoGithubUserName _ : _) =
	Left "Duplicate GitHub user options"
checkDupOption (FoGithubToken _ : FoGithubToken _ : _) =
	Left "Duplicate GitHub token options"
checkDupOption (o : os) = (o :) <$> checkDupOption os

followboxOptionToInfo :: [FollowboxOption] -> IO (Either String FollowboxInfo)
followboxOptionToInfo [] = pure $ Right FollowboxInfo {
	fiBrowser = defaultBrowser, fiGithubUserNameToken = Nothing }
followboxOptionToInfo (FoBrowser brs : os) =
	((\fi -> fi { fiBrowser = brs }) <$>) <$> followboxOptionToInfo os
followboxOptionToInfo (FoGithubUserName un : FoGithubToken fp : os) = do
	r <- followboxOptionToInfo os
	tkn <- removeLastNL <$> BS.readFile fp
	pure $ (\fi -> fi { fiGithubUserNameToken = Just (un, tkn) }) <$> r
followboxOptionToInfo _ = pure $ Left "Bad option set"

followboxOptions :: [OptDescr FollowboxOption]
followboxOptions = [
	Option "b" ["browser"] (ReqArg FoBrowser "browser")
		"Set browser to access GitHub user page",
	Option "u" ["github-user-name"]
		(ReqArg (FoGithubUserName . BSC.pack) "GitHub user name")
		"Set GitHub user name",
	Option "t" ["github-token"]
		(ReqArg FoGithubToken "GitHub token") "Set Github token" ]

getFollowboxInfo :: IO (Either String FollowboxInfo)
getFollowboxInfo = do
	args <- getArgs
	let	(os, args', errs) = getOpt Permute followboxOptions args
	case (args', errs) of
		([], []) -> do
			let	os' = checkDupOption $ sort os
			either (pure . Left) followboxOptionToInfo os'
		(_, []) -> pure $ Left "No options are permited"
		_ -> pure . Left $ concat errs

removeLastNL :: BS.ByteString -> BS.ByteString
removeLastNL ba
	| Just (bs, '\n') <- BSC.unsnoc ba = bs
	| otherwise = ba
