{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Plugin.TypeCheck.Nat.Simple where

import GhcPlugins
import TcPluginM
import TcRnTypes
import Control.Monad
import Control.Monad.Trans.Except

import Data.Bool
import Data.Either
import TcEvidence
import TyCoRep

import Plugin.TypeCheck.Nat.Simple.Decode
import Data.Derivation.CanDerive

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
	pure $ TcPluginOk (rights $ runExcept . result gs <$> ws) []

result :: [Ct] -> Ct -> Except Message (EvTerm, Ct)
result gs w = unNomEq w >>= \(l, r) ->
	bool (throwE em) (pure (et l r, w)) . canDerive g =<< wnt =<< decode l r
	where
	em = "result: fail"
	g = mkGiven . rights $ runExcept . (uncurry decode =<<) . unNomEq <$> gs
	wnt = maybe (throwE "mkWanted: fail") pure . mkWanted
	et l r = EvExpr . Coercion $
		mkUnivCo (PluginProv "Plugin.TypeCheck.Nat.Simple") Nominal l r

unNomEq :: Ct -> Except Message (Type, Type)
unNomEq ct = case classifyPredType . ctEvPred $ ctEvidence ct of
	EqPred NomEq l r -> pure (l, r)
	_ -> throwE "Cannot unNomEq"
