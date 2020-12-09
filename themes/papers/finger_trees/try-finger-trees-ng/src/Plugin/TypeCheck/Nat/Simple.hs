{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Plugin.TypeCheck.Nat.Simple (plugin) where

import GhcPlugins hiding ((<>))
import TcPluginM
import TcRnTypes
import TcEvidence
import TyCoRep
import Data.Bool
import Data.Maybe
import Data.Either

import Plugin.TypeCheck.Nat.Simple.Decode
import Data.Derivation.CanDerive

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
		. foldr (<>) (Left "") $ decode <$> ws
--		. ctToExpEq $ head ws
	let	gs' = mkGiven . catMaybes $ either (const Nothing) Just . decode <$> gs
	tcPluginTrace "Given: " $ ppr gs'
--	tcPluginTrace "Wanted Expression2: " . ppr
--		$ either (const (Nothing, [])) expToWanted . decode <$> ws
--	tcPluginTrace "Wanted Expression2: " . ppr . ((canDeriveGen (mkGiven []) <$>) <$>)
--		$ either (const Nothing) (fst . expToWanted) . decode <$> ws
	tcPluginTrace "Oh Gosh!: " . ppr $ canDeriveCt gs <$> ws
	pure $ TcPluginOk (rights $ canDeriveCt gs <$> ws) []

canDeriveCt :: [Ct] -> Ct -> Either Message (EvTerm, Ct)
canDeriveCt gs w = do
	et <- evTerm w
	let	gs' = mkGiven . catMaybes $ either (const Nothing) Just . decode <$> gs
	bool (Left $ "foo: " <> Message (showSDocUnsafe $ ppr gs')) (pure (et, w)) . canDerive gs'
		=<< maybe (Left "bar") Right . mkWanted =<< decode w

evTerm :: Ct -> Either Message EvTerm
evTerm w = do
	(t1, t2) <- unNomEq w
	pure $ mkEvTerm t1 t2

mkEvTerm :: Type -> Type -> EvTerm
mkEvTerm t1 t2 = EvExpr . Coercion
	$ mkUnivCo (PluginProv "TypeCheck.Nat") Nominal t1 t2
