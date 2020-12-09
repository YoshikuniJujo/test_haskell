{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Plugin.TypeCheck.Nat.Simple (plugin) where

import GhcPlugins (
	Plugin(..), defaultPlugin, Var, Expr(..),
	PredTree(..), EqRel(..), classifyPredType, mkUnivCo, ppr )
import TcPluginM (TcPluginM, tcPluginTrace)
import TcRnTypes (TcPlugin(..), TcPluginResult(..), Ct, ctEvPred, ctEvidence)
import TcEvidence (EvTerm(..), Role(..))
import TyCoRep (Type, UnivCoProvenance(..))
import Control.Monad.Trans.Except (runExcept)
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
--	tcPluginTrace "Given: " $ ppr gs
--	tcPluginTrace "Derived: " $ ppr ds
	tcPluginTrace "Wanted: " $ ppr ws
	tcPluginTrace "Wanted Expression: " . ppr
		. foldr (<>) (Left "") $ decodeCt <$> ws
--		. ctToExpEq $ head ws
	let	gs' = mkGiven . catMaybes $ either (const Nothing) Just . decodeCt <$> gs
	tcPluginTrace "Given: " $ ppr gs'
--	tcPluginTrace "Wanted Expression2: " . ppr
--		$ either (const (Nothing, [])) expToWanted . decode <$> ws
--	tcPluginTrace "Wanted Expression2: " . ppr . ((canDeriveGen (mkGiven []) <$>) <$>)
--		$ either (const Nothing) (fst . expToWanted) . decode <$> ws
	tcPluginTrace "Oh Gosh!: " . ppr $ canDeriveCt gs <$> ws
	pure $ TcPluginOk (rights $ canDeriveCt gs <$> ws) []

decodeCt :: Ct -> Either Message (Exp Var Bool)
decodeCt = ctToExpEq

ctToExpEq :: Ct -> Either Message (Exp Var Bool)
ctToExpEq ct = do
	(t1, t2) <- unNomEq ct
	runExcept $ decode t1 t2

unNomEq :: Ct -> Either Message (Type, Type)
unNomEq ct = case classifyPredType . ctEvPred $ ctEvidence ct of
	EqPred NomEq t1 t2 -> Right (t1, t2)
	EqPred foo t1 t2 -> Left $ Message (ppr foo) <> " " <>
		Message (ppr t1) <> " " <> Message (ppr t2) <> " Cannot unNOmEq"
	_ -> Left $ "Cannot unNomEq"

canDeriveCt :: [Ct] -> Ct -> Either Message (EvTerm, Ct)
canDeriveCt gs w = do
	(t1, t2) <- unNomEq w
	let	gs' = mkGiven . catMaybes $ either (const Nothing) Just . decodeCt <$> gs
	bool (Left $ "foo: " <> Message (ppr gs')) (pure (mkEvTerm t1 t2, w)) . canDerive gs'
		=<< maybe (Left "bar") Right . mkWanted =<< runExcept (decode t1 t2)

evTerm :: Ct -> Either Message EvTerm
evTerm w = do
	(t1, t2) <- unNomEq w
	pure $ mkEvTerm t1 t2

mkEvTerm :: Type -> Type -> EvTerm
mkEvTerm t1 t2 = EvExpr . Coercion
	$ mkUnivCo (PluginProv "TypeCheck.Nat") Nominal t1 t2
