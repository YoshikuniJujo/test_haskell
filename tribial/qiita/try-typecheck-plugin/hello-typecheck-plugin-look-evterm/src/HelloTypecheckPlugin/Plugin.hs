{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module HelloTypecheckPlugin.Plugin where

import Data.Maybe

import GhcPlugins
import TcPluginM
import TcRnTypes

import TyCoRep
import TcEvidence

plugin :: Plugin
plugin = defaultPlugin {
	installCoreToDos = showCore,
	tcPlugin = const . Just $ TcPlugin {
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
--	return $ TcPluginOk [] []
	return $ TcPluginOk (catMaybes $ mkEvTerm <$> ws) []

mkEvTerm :: Ct -> Maybe (EvTerm, Ct)
mkEvTerm ct@(CNonCanonical (CtWanted (TyConApp _ [_, _, a, i]) _ _ _)) = Just (
	EvExpr . Coercion
		$ mkUnivCo (PluginProv "!HelloGadtsWithInt.Plugin") Nominal a i,
	ct)
mkEvTerm _ = Nothing

showCore :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
showCore _ = return . (corePlugin :)

corePlugin :: CoreToDo
corePlugin = CoreDoPluginPass "!HelloTypecheckPlugin.Plugin" $ \mg -> do
	putMsg . ppr $ mg_binds mg
	return mg
