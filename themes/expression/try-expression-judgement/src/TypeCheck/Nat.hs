{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TypeCheck.Nat where

import GhcPlugins

import TcPluginM
import TcRnTypes

plugin :: Plugin
plugin = defaultPlugin { tcPlugin = const . Just $ TcPlugin {
	tcPluginInit = return (),
	tcPluginSolve = const solveNat,
	tcPluginStop = const $ return () } }

solveNat :: [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
solveNat gs _ ws = do
	tcPluginTrace "!TypeCheck.Nat:" ""
	tcPluginTrace "Given:" $ ppr gs
	tcPluginTrace "Wanted:" $ ppr ws
	return $ TcPluginOk [] []
