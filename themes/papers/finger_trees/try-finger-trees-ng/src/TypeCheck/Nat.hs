{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TypeCheck.Nat (plugin) where

import Data.Bool
import Data.Either

import GhcPlugins

import TcPluginM
import TcRnTypes

import TcEvidence

import Given
import Expression
import TypeCheck.Nat.Orphans ()

import TypeCheck.Nat.CanDeriveCt2
import TypeCheck.Nat.Common

plugin :: Plugin
plugin = defaultPlugin { tcPlugin = const . Just $ TcPlugin {
	tcPluginInit = return (),
	tcPluginSolve = const solveNat,
	tcPluginStop = const $ return () } }

solveNat :: [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
solveNat gs _ ws = do
	let	oks = rights (canDeriveCt gs <$> ws)
		oks2 = rights (canDeriveCt2 gs <$> ws)
	pure $ TcPluginOk (oks ++ oks2) []

canDeriveCt :: [Ct] -> Ct -> Either String (EvTerm, Ct)
canDeriveCt gs w = do
	(t1, t2) <- getTypes w
	bool (Left "foo") (pure (makeEvTerm t1 t2, w))
		=<< canDerive given . Given.Wanted =<< expression w
	where given = Given.Given . rights $ expression <$> gs

expression :: Ct -> Either String (Expression Integer Var)
expression ct = do
	(t1, t2) <- getTypes ct
	e1 <- typeToExpression t1
	e2 <- typeToExpression t2
	return . reductAndNormalizeSign $ e1 .- e2
