{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TypeCheck.Nat where

import Data.Maybe
import Data.Either

import GhcPlugins

import TcPluginM
import TcRnTypes

import TyCoRep
import TcTypeNats
import TcEvidence

import Given
import Expression

plugin :: Plugin
plugin = defaultPlugin { tcPlugin = const . Just $ TcPlugin {
	tcPluginInit = return (),
	tcPluginSolve = const solveNat,
	tcPluginStop = const $ return () } }

solveNat :: [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
solveNat gs _ ws = do
	tcPluginTrace "!TypeCheck.Nat:" ""
	tcPluginTrace "!Given:" $ ppr gs
	tcPluginTrace "!Wanted:" $ ppr ws
	(tcPluginTrace "!Types" . ppr) `mapM_` catMaybes (getTypes <$> ws)
	(tcPluginTrace "!Expression Given" . ppr) (Given.Given $ catMaybes (expression <$> gs))
	(tcPluginTrace "!Expression Wanted" . ppr . Given.Wanted) `mapM_` catMaybes (expression <$> ws)
	(tcPluginTrace "!CanDerive" . ppr) `mapM_` (canDeriveCt gs <$> ws)
	let	oks = rights $ catMaybes (canDeriveCt gs <$> ws)
	return $ TcPluginOk oks []

canDeriveCt :: [Ct] -> Ct -> Maybe (Either Ct (EvTerm, Ct))
canDeriveCt gs_ w_ = do
	(t1, t2) <- getTypes w_
	let	gs = givenExpression gs_
	w <- wantedExpression w_
	b <- canDerive gs w
	if b then return $ Right (makeEvTerm t1 t2, w_) else return $ Left w_

makeEvTerm :: Type -> Type -> EvTerm
makeEvTerm t1 t2 = EvExpr . Coercion
	$ mkUnivCo (PluginProv "TypeCheck.Nat") Nominal t1 t2

givenExpression :: [Ct] -> Given.Given Integer Var
givenExpression gs = Given.Given . catMaybes $ expression <$> gs

wantedExpression :: Ct -> Maybe (Given.Wanted Integer Var)
wantedExpression w = Given.Wanted <$> expression w

getTypes :: Ct -> Maybe (Type, Type)
getTypes ct = case classifyPredType . ctEvPred $ ctEvidence ct of
	EqPred NomEq t1 t2 -> Just (t1, t2); _ -> Nothing

expression :: Ct -> Maybe (Expression Integer Var)
expression ct = do
	(t1, t2) <- getTypes ct
	e1 <- typeToExpression t1
	e2 <- typeToExpression t2
	return . reduct $ e1 .- e2

typeToExpression :: Type -> Maybe (Expression Integer Var)
typeToExpression (TyVarTy v) = Just $ var v
typeToExpression (LitTy (NumTyLit n)) = Just $ num n
typeToExpression (TyConApp tc [a, b])
	| tc == typeNatAddTyCon = do
		ea <- typeToExpression a
		eb <- typeToExpression b
		return $ ea .+ eb
	| tc == typeNatSubTyCon = do
		ea <- typeToExpression a
		eb <- typeToExpression b
		return $ ea .- eb
typeToExpression _ = Nothing
