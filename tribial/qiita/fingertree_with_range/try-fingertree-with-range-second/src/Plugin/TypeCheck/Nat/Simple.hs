{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Plugin.TypeCheck.Nat.Simple where

import GhcPlugins
import TcPluginM
import TcRnTypes

import Control.Monad.Trans.Except
import Control.Monad

import Plugin.TypeCheck.Nat.Simple.Decode

plugin :: Plugin
plugin = defaultPlugin { tcPlugin = const $ Just TcPlugin {
	tcPluginInit = pure (),
	tcPluginSolve = const solve,
	tcPluginStop = const $ pure () } }

solve :: [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
solve _ _ [] = pure $ TcPluginOk [] []
solve gs ds ws = do
	tcPluginTrace "!Plugin.TypeCheck.Nat.Simple" ""
	tcPluginTrace "Given: " . ppr $ runExcept . (uncurry decode <=< unNomEq) <$> gs
	tcPluginTrace "Derived: " . ppr $ runExcept . (uncurry decode <=< unNomEq) <$> ds
	tcPluginTrace "Wanted: " . ppr $ runExcept . (uncurry decode <=< unNomEq) <$> ws
	pure $ TcPluginOk [] []

unNomEq :: Ct -> Except Message (Type, Type)
unNomEq ct = case classifyPredType . ctEvPred $ ctEvidence ct of
	EqPred NomEq l r -> pure (l, r)
	_ -> throwE "Cannot unNomEq"
