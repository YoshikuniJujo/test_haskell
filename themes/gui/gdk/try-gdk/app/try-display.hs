{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Exception
import System.Environment
import System.Console.GetOpt

import Graphics.Gdk.General
import Graphics.Gdk.GdkDisplay
import Graphics.Gdk.Exception

main :: IO ()
main = do
	(print =<< gdkDisplayGetDefault) `catch` \(e :: GdkNoDefaultDisplay) -> print e
	(_pn, args) <- join $ gdkInit <$> getProgName <*> getArgs
	let	(os, as, es) = getOpt Permute optDescrs args
		sts = optionsToSettings os
	print sts
	print as
	print es
	dd <- gdkDisplayGetDefault
	print $ gdkDisplayGetName dd
	case settingsMyDisplayName sts of
		Just n -> do
			nd <- gdkDisplayOpen n
			print $ gdkDisplayGetName nd
			gdkDisplayClose nd
		Nothing -> pure ()

data Settings = Settings {
	settingsMyDisplayName :: Maybe String
	} deriving Show

initialSettings :: Settings
initialSettings = Settings {
	settingsMyDisplayName = Nothing
	}

data Option
	= MyDisplayName String
	deriving Show

optDescrs :: [OptDescr Option]
optDescrs = [
	Option "" ["my-display"] (ReqArg MyDisplayName "My display name")
		"Set my display name"
	]

optionSet :: Option -> Settings -> Settings
optionSet (MyDisplayName dn) s = s { settingsMyDisplayName = Just dn }

optionsToSettings :: [Option] -> Settings
optionsToSettings [] = initialSettings
optionsToSettings (o : os) = optionSet o $ optionsToSettings os
