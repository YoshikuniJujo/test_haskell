{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Plugin.TypeCheck.Nat.Simple (plugin) where

import GhcPlugins (
	Plugin(..), defaultPlugin, Expr(..),
	PredTree(..), EqRel(..), classifyPredType, mkUnivCo, ppr )
import TcPluginM (TcPluginM, tcPluginTrace)
import TcRnTypes (TcPlugin(..), TcPluginResult(..), Ct, ctEvPred, ctEvidence)
import TcEvidence (EvTerm(..), Role(..))
import TyCoRep (Type, UnivCoProvenance(..))
import Control.Monad.Trans.Except (Except, runExcept, throwE)
import Data.Bool (bool)
import Data.Either (rights)

import Plugin.TypeCheck.Nat.Simple.Decode (decode, Message(..))
import Data.Derivation.CanDerive (canDerive, mkGiven, mkWanted)

---------------------------------------------------------------------------

plugin :: Plugin
plugin = defaultPlugin { tcPlugin = const $ Just TcPlugin {
	tcPluginInit = pure (), tcPluginSolve = const solve,
	tcPluginStop = const $ pure () } }

solve :: [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
solve _ _ [] = pure $ TcPluginOk [] []
solve gs ds ws = do
	tcPluginTrace "!Plugin.TypeCheck.Nat.Simple" ""
	tcPluginTrace "Given: " $ ppr gs
	tcPluginTrace "Derived: " $ ppr ds
	tcPluginTrace "Wanted: " $ ppr ws
	pure $ TcPluginOk (rights $ runExcept . result gs <$> ws) []

result :: [Ct] -> Ct -> Except Message (EvTerm, Ct)
result gs w = unNomEq w >>= \(l, r) ->
	bool (throwE em) (pure (et l r, w)) . canDerive g =<< wnt =<< decode l r
	where
	em = "result: cannot derive:" <> "from" <> Message (ppr g)
	g = mkGiven . rights $ runExcept . (uncurry decode =<<) . unNomEq <$> gs
	wnt = maybe (throwE "mkWanted: fail") pure . mkWanted
	et l r = EvExpr . Coercion $
		mkUnivCo (PluginProv "Plugin.TypeCheck.Nat.Simple") Nominal l r

unNomEq :: Ct -> Except Message (Type, Type)
unNomEq ct = case classifyPredType . ctEvPred $ ctEvidence ct of
	EqPred NomEq l r -> pure (l, r)
	EqPred er l r -> throwE $ "Cannot unNomEq:" <>
		Message (ppr er) <> Message (ppr l) <> Message (ppr r)
	_ -> throwE "Cannot unNomEq"
