{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module New.TypeCheck.Nat (plugin) where

import GhcPlugins hiding ((<>))
import TcPluginM
import TcRnTypes

import New.TypeCheck.Nat.Decode

plugin :: Plugin
plugin = defaultPlugin { tcPlugin = const . Just $ TcPlugin {
	tcPluginInit = pure (),
	tcPluginSolve = const solveNat,
	tcPluginStop = const $ pure () } }

solveNat :: [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
solveNat _ _ [] = do
	tcPluginTrace "!New.TypeCheck.Nat.plugin" ""
	pure $ TcPluginOk [] []
solveNat gs ds ws = do
	tcPluginTrace "!New.TypeCheck.Nat.Plugin" ""
	tcPluginTrace "Given: " $ ppr gs
	tcPluginTrace "Derived: " $ ppr ds
	tcPluginTrace "Wanted: " $ ppr ws
	tcPluginTrace "Wanted Expression: " . ppr
		. foldr (<>) (Left "") $ ctToExpEq <$> ws
--		. ctToExpEq $ head ws
	pure $ TcPluginOk [] []

instance Show Var where show = showSDocUnsafe . ppr
