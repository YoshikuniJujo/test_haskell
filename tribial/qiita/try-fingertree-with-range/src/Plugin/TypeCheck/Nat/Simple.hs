{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Plugin.TypeCheck.Nat.Simple where

import GHC.Plugins
import GHC.Tc.Plugin
import GHC.Tc.Types
import GHC.Tc.Types.Constraint
import GHC.Core.Predicate
import Control.Monad
import Control.Monad.Trans.Except

import Plugin.TypeCheck.Nat.Simple.Decode

plugin :: Plugin
plugin = defaultPlugin { tcPlugin = const $ Just TcPlugin {
	tcPluginInit = pure (),
	tcPluginSolve = const solve,
	tcPluginStop = const $ pure () } }

solve :: [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
solve _ _ [] = pure $ TcPluginOk [] []
solve gs ds ws = do
	oc <- lookupOrdCond
	tcPluginTrace "!Plugin.TypeCheck.Nat.Simple" ""
	tcPluginTrace "Given: " . ppr $ runExcept . (uncurry (decode oc) <=< unNomEq) <$> gs
	tcPluginTrace "Derived: " . ppr $ runExcept . (uncurry (decode oc) <=< unNomEq) <$> ds
	tcPluginTrace "Wanted: " . ppr $ runExcept . (uncurry (decode oc) <=< unNomEq) <$> ws
	pure $ TcPluginOk [] []

unNomEq :: Ct -> Except Message (Type, Type)
unNomEq ct = case classifyPredType . ctEvPred $ ctEvidence ct of
	EqPred NomEq l r -> pure (l, r)
	_ -> throwE "Cannot unNomEq"
