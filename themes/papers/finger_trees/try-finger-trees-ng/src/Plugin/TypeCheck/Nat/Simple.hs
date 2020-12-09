{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Plugin.TypeCheck.Nat.Simple (plugin) where

import GhcPlugins (
	Plugin(..), defaultPlugin, Var, Expr(..),
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
import Data.Derivation.Expression (Exp)

---------------------------------------------------------------------------

plugin :: Plugin
plugin = defaultPlugin { tcPlugin = const . Just $ TcPlugin {
	tcPluginInit = pure (),
	tcPluginSolve = const solveNat,
	tcPluginStop = const $ pure () } }

solveNat :: [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
solveNat _ _ [] = do
	tcPluginTrace "!TypeCheck.Nat.plugin" ""
	pure $ TcPluginOk [] []
solveNat gs ds ws = do
	tcPluginTrace "!TypeCheck.Nat.Plugin" ""
	tcPluginTrace "Given: " $ ppr gs
	tcPluginTrace "Derived: " $ ppr ds
	tcPluginTrace "Wanted: " $ ppr ws
	tcPluginTrace "Wanted Expression: " . ppr
		. foldr (<>) (Left "") $ runExcept . decodeCt <$> ws
	let	gs' = mkGiven . catMaybes $ either (const Nothing) Just . runExcept . decodeCt <$> gs
	tcPluginTrace "Given: " $ ppr gs'
	tcPluginTrace "Oh Gosh!: " . ppr $ runExcept . canDeriveCt gs <$> ws
	pure $ TcPluginOk (rights $ runExcept . canDeriveCt gs <$> ws) []

decodeCt :: Ct -> Except Message (Exp Var Bool)
decodeCt = ctToExpEq

ctToExpEq :: Ct -> Except Message (Exp Var Bool)
ctToExpEq ct = uncurry decode =<< unNomEq ct

unNomEq :: Ct -> Except Message (Type, Type)
unNomEq ct = case classifyPredType . ctEvPred $ ctEvidence ct of
	EqPred NomEq t1 t2 -> pure (t1, t2)
	EqPred foo t1 t2 -> throwE $ Message (ppr foo) <> " " <>
		Message (ppr t1) <> " " <> Message (ppr t2) <> " Cannot unNOmEq"
	_ -> throwE $ "Cannot unNomEq"

canDeriveCt :: [Ct] -> Ct -> Except Message (EvTerm, Ct)
canDeriveCt gs w = do
	(t1, t2) <- unNomEq w
	let	gs' = mkGiven . catMaybes $ either (const Nothing) Just . runExcept . decodeCt <$> gs
	bool (throwE $ "foo: " <> Message (ppr gs')) (pure (mkEvTerm t1 t2, w)) . canDerive gs'
		=<< maybe (throwE "bar") pure . mkWanted =<< decode t1 t2

mkEvTerm :: Type -> Type -> EvTerm
mkEvTerm t1 t2 = EvExpr . Coercion
	$ mkUnivCo (PluginProv "TypeCheck.Nat") Nominal t1 t2
