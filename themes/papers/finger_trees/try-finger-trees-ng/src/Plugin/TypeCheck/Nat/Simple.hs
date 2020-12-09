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
import Data.Maybe (catMaybes)
import Data.Either (rights)

import Plugin.TypeCheck.Nat.Simple.Decode (decode, Message(..))
import Data.Derivation.CanDerive (canDerive, mkGiven, mkWanted)

---------------------------------------------------------------------------

plugin :: Plugin
plugin = defaultPlugin { tcPlugin = const . Just $ TcPlugin {
	tcPluginInit = pure (), tcPluginSolve = const solve,
	tcPluginStop = const $ pure () } }

solve :: [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
solve gs ds ws = do
	tcPluginTrace "!TypeCheck.Nat.Plugin" ""
	tcPluginTrace "Given: " $ ppr gs
	tcPluginTrace "Derived: " $ ppr ds
	tcPluginTrace "Wanted: " $ ppr ws
	pure $ TcPluginOk (rights $ runExcept . result gs <$> ws) []

result :: [Ct] -> Ct -> Except Message (EvTerm, Ct)
result gs w = do
	(l, r) <- unNomEq w
	let	gs' = mkGiven . catMaybes $ either (const Nothing) Just . runExcept . decodeCt <$> gs
	bool (throwE $ "foo: " <> Message (ppr gs')) (pure (mkEvTerm l r, w)) . canDerive gs'
		=<< maybe (throwE "bar") pure . mkWanted =<< decode l r
	where
	mkEvTerm l r = EvExpr . Coercion
		$ mkUnivCo (PluginProv "TypeCheck.Nat") Nominal l r
	decodeCt ct = uncurry decode =<< unNomEq ct

unNomEq :: Ct -> Except Message (Type, Type)
unNomEq ct = case classifyPredType . ctEvPred $ ctEvidence ct of
	EqPred NomEq l r -> pure (l, r)
	EqPred foo l r -> throwE $ Message (ppr foo) <> " " <>
		Message (ppr l) <> " " <> Message (ppr r) <> " Cannot unNOmEq"
	_ -> throwE $ "Cannot unNomEq"
