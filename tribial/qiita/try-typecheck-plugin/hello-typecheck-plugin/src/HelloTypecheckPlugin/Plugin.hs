{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module HelloTypecheckPlugin.Plugin where

import GhcPlugins
import TcPluginM
import TcRnTypes

plugin :: Plugin
plugin = defaultPlugin { tcPlugin = const . Just $ TcPlugin {
	tcPluginInit = return (),
	tcPluginSolve = const solveHello,
	tcPluginStop = const $ return () } }

solveHello :: [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
solveHello _ _ [] = do
	tcPluginTrace "!HelloTypecheckPlugin.Plugin:" ""
	return $ TcPluginOk [] []
solveHello gs _ ws = do
	tcPluginTrace "!HelloTypecheckPlugin.Plugin:" ""
	tcPluginTrace "Given: " $ ppr gs
	tcPluginTrace "Wanted: " $ ppr ws
	return $ TcPluginOk [] []
