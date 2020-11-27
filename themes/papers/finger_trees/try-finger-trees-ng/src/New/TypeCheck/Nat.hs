{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module New.TypeCheck.Nat (plugin) where

import GhcPlugins hiding ((<>))
import TcPluginM
import TcRnTypes
import TcEvidence
import TyCoRep
import Data.Bool
import Data.Maybe
import Data.Either

import New.TypeCheck.Nat.Decode
import New.Polynominal.Given
import New.Polynominal.Wanted
import New.Polynominal.Derive

plugin :: Plugin
plugin = defaultPlugin { tcPlugin = const . Just $ TcPlugin {
	tcPluginInit = pure (),
	tcPluginSolve = const solveNat,
	tcPluginStop = const $ pure () } }

solveNat :: [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
solveNat _ _ [] = do
	tcPluginTrace "!New.TypeCheck.Nat.plugin" ""
	pure $ TcPluginOk [] []
solveNat gs ds ws = do
	tcPluginTrace "!New.TypeCheck.Nat.Plugin" ""
--	tcPluginTrace "Given: " $ ppr gs
--	tcPluginTrace "Derived: " $ ppr ds
	tcPluginTrace "Wanted: " $ ppr ws
	tcPluginTrace "Wanted Expression: " . ppr
		. foldr (<>) (Left "") $ ctToExpEq <$> ws
--		. ctToExpEq $ head ws
	let	gs' = expsToGiven . catMaybes $ either (const Nothing) Just . decode <$> gs
	tcPluginTrace "Given: " $ ppr gs'
	tcPluginTrace "Wanted Expression2: " . ppr
		$ either (const Nothing) expToWanted . decode <$> ws
	tcPluginTrace "Wanted Expression2: " . ppr . ((canDerive (expsToGiven []) <$>) <$>)
		$ either (const Nothing) expToWanted . decode <$> ws
	tcPluginTrace "Oh Gosh!: " . ppr $ canDeriveCt gs <$> ws
	pure $ TcPluginOk (rights $ canDeriveCt gs <$> ws) []

instance Show Var where show = showSDocUnsafe . ppr

canDeriveCt :: [Ct] -> Ct -> Either String (EvTerm, Ct)
canDeriveCt gs w = do
	(t1, t2) <- unNomEq w
	let	gs' = expsToGiven . catMaybes $ either (const Nothing) Just . decode <$> gs
	w' <- maybe (Left "foo") Right . expToWanted =<< decode w
	bool (Left "foo") (pure (makeEvTerm t1 t2, w)) (canDerive gs' w')

makeEvTerm :: Type -> Type -> EvTerm
makeEvTerm t1 t2 = EvExpr . Coercion
	$ mkUnivCo (PluginProv "New.TypeCheck.Nat") Nominal t1 t2
