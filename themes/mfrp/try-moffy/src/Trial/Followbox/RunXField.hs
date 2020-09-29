{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Followbox.RunXField (
	-- * Run Follow Box
	runFollowbox, evalFollowbox ) where

import Control.Moffy
import Control.Moffy.Run (interpretSt)
import Data.List (sort)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Console.GetOpt (ArgOrder(..), OptDescr(..), ArgDescr(..), getOpt)
import System.Random (mkStdGen)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import Trial.Followbox.ViewType

import Trial.Followbox.Event (FollowboxEv)
import Trial.Followbox.XFieldHandle (
	handleFollowbox, FollowboxState, initialFollowboxState )
import Trial.Followbox.TypeSynonym (
	WindowTitle, Browser, GithubNameToken, GithubUserName )
import Field (
	Field, openField, closeField, flushField, clearField, exposureMask, buttonPressMask )

import Control.Moffy.View.XField
import Control.Moffy.Event.DefaultWindow
import Data.Type.Set
import Data.OneOfThem

---------------------------------------------------------------------------

-- * DEFAULT BROWSER
-- * RUN FOLLOWBOX
-- * GET FOLLOWBOX INFO
-- * GET OPT

---------------------------------------------------------------------------
-- DEFAULT BROWSER
---------------------------------------------------------------------------

defaultBrowser :: Browser
defaultBrowser = "firefox"

---------------------------------------------------------------------------
-- RUN FOLLOWBOX
---------------------------------------------------------------------------

runFollowbox :: WindowTitle -> Sig s (StoreDefaultWindow :- FollowboxEv) View a -> IO (a, FollowboxState)
runFollowbox ttl s =
	either ((>> exitFailure) . putStrLn) (run ttl s) =<< getFollowboxInfo

evalFollowbox :: WindowTitle -> Sig s (StoreDefaultWindow :- FollowboxEv) View a -> IO a
evalFollowbox = ((fst <$>) .) . runFollowbox

run :: WindowTitle -> Sig s (StoreDefaultWindow :- FollowboxEv) View a -> FollowboxInfo -> IO (a, FollowboxState)
run ttl s FollowboxInfo { fiBrowser = brs, fiGithubUserNameToken = mgnt } =
	openField ttl [exposureMask, buttonPressMask] >>= \f ->
		interpretSt (handleFollowbox f brs mgnt) (view f) s
			(initialFollowboxState $ mkStdGen 8) <* closeField f

view :: Field -> View -> IO ()
view f (View v) = clearField f >> view1 f `mapM` v >> flushField f

view1 :: Field -> View1 -> IO ()
view1 f = apply $ viewText f >-- viewLine f >-- SingletonFun (viewImage f)

---------------------------------------------------------------------------
-- GET FOLLOWBOX INFO
---------------------------------------------------------------------------

data FollowboxInfo = FollowboxInfo {
	fiBrowser :: Browser, fiGithubUserNameToken :: Maybe GithubNameToken }

getFollowboxInfo :: IO (Either String FollowboxInfo)
getFollowboxInfo = do
	(os, args, ems) <- getOpt Permute followboxOptions <$> getArgs
	case (args, ems) of
		([], []) ->
			either (pure . Left) optionsToInfo . chkDupOpt $ sort os
		(_, []) -> pure $ Left "Only options are permitted"
		_ -> pure . Left $ concat ems

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
chkDupOpt (FoBrowser _ : FoBrowser _ : _) = Left "Duplicate Browser options"
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
