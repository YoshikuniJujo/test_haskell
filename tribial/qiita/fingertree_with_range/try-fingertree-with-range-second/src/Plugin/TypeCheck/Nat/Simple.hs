{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Plugin.TypeCheck.Nat.Simple where

import GhcPlugins
import TcPluginM
import TcRnTypes

plugin :: Plugin
plugin = defaultPlugin { tcPlugin = const $ Just TcPlugin {
	tcPluginInit = pure (),
	tcPluginSolve = const solve,
	tcPluginStop = const $ pure () } }

solve :: [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
solve _ _ [] = pure $ TcPluginOk [] []
solve gs ds ws = do
	tcPluginTrace "!Plugin.TypeCheck.Nat.Simple" ""
	tcPluginTrace "Given: " $ ppr gs
	tcPluginTrace "Derived: " $ ppr ds
	tcPluginTrace "Wanted: " $ ppr ws
	pure $ TcPluginOk [] []
