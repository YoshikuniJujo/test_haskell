{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TypeCheck where

import GhcPlugins
import TcPluginM
import TcRnTypes
import Class
import TcEvidence

import TyCoRep

plugin :: Plugin
plugin = defaultPlugin { tcPlugin = const . Just $ TcPlugin {
	tcPluginInit = pure (),
	tcPluginSolve = const solve,
	tcPluginStop = const $ pure () } }

solve :: [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
solve gs _ ws = do
	tcPluginTrace "!TypeCheck" ""
	tcPluginTrace "Givens: " $ ppr gs
	tcPluginTrace "Wanteds: " $ ppr ws
	tcPluginTrace "Givens 1: " . ppr $ unClassPred <$> gs
	tcPluginTrace "Wanteds 1: " . ppr $ unClassPred <$> ws
	tcPluginTrace "Givens 2: " . ppr $ (uncurry mkEvScSelectors <$>) . unClassPred <$> gs
	tcPluginTrace "Wanteds 2: " . ppr $ (uncurry mkEvScSelectors <$>) . unClassPred <$> ws
	let	zero = LitTy $ NumTyLit 0
		evZero = EvExpr . Coercion $ mkUnivCo (PluginProv "TypeCheck") Nominal zero zero
	tcPluginTrace "test: " $ ppr evZero
	pure $ TcPluginOk [] []

unClassPred :: Ct -> Maybe (Class, [Type])
unClassPred ct = case classifyPredType . ctEvPred $ ctEvidence ct of
	ClassPred c ts -> Just (c, ts)
	_ -> Nothing
--	et = ((EvExpr . Coercion) .) . mkUnivCo (PluginProv hd) Nominal
