{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TypeCheck.Nat where

import Data.Either

import GhcPlugins

import TcPluginM
import TcRnTypes

import TyCoRep
import TcTypeNats
import TcEvidence

import qualified Data.Text as T

import Given
import Expression
import TypeCheck.Nat.Orphans

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
	(tcPluginTrace "!Types" . ppr) `mapM_` rights (getTypes <$> ws)
	(tcPluginTrace "!Types" . ppr) `mapM_` (wantedExpression <$> ws)
	(tcPluginTrace "!Expression Given" . ppr) (Given.Given $ rights (expression <$> gs))
	(tcPluginTrace "!Expression Wanted" . ppr . Given.Wanted) `mapM_` rights (expression <$> ws)
	(tcPluginTrace "!Expression Wanted" . ppr) `mapM_` (expression <$> ws)
	(tcPluginTrace "!CanDerive" . ppr) `mapM_` (canDeriveCt gs <$> ws)
	let	oks = rights $ rights (canDeriveCt gs <$> ws)
	return $ TcPluginOk oks []

canDeriveCt :: [Ct] -> Ct -> Either T.Text (Either Ct (EvTerm, Ct))
canDeriveCt gs_ w_ = either (Left . T.pack) Right $ do
	(t1, t2) <- getTypes w_
	let	gs = givenExpression gs_
	w <- wantedExpression w_
	b <- canDerive gs w
	if b then return $ Right (makeEvTerm t1 t2, w_) else return $ Left w_

makeEvTerm :: Type -> Type -> EvTerm
makeEvTerm t1 t2 = EvExpr . Coercion
	$ mkUnivCo (PluginProv "TypeCheck.Nat") Nominal t1 t2

givenExpression :: [Ct] -> Given.Given Integer Var
givenExpression gs = Given.Given . rights $ expression <$> gs

wantedExpression :: Ct -> Either String (Given.Wanted Integer Var)
wantedExpression w = Given.Wanted <$> expression w

getTypes :: Ct -> Either String (Type, Type)
getTypes ct = case classifyPredType . ctEvPred $ ctEvidence ct of
	EqPred NomEq t1 t2 -> Right (t1, t2);
	EqPred foo t1 t2 -> Left $ showSDocUnsafe (ppr foo) ++ " " ++ showSDocUnsafe (ppr t1) ++ " " ++ showSDocUnsafe (ppr t2) ++ "Cannot get types"
	_ -> Left "Cannot get types"

expression :: Ct -> Either String (Expression Integer Var)
expression ct = do
	(t1, t2) <- getTypes ct
	e1 <- typeToExpression t1
	e2 <- typeToExpression t2
	return . reduct $ e1 .- e2

typeToExpression :: Type -> Either String (Expression Integer Var)
typeToExpression (TyVarTy v) = Right $ var v
typeToExpression (LitTy (NumTyLit n)) = Right $ num n
typeToExpression (TyConApp tc [a, b])
	| tc == typeNatAddTyCon = do
		ea <- typeToExpression a
		eb <- typeToExpression b
		return $ ea .+ eb
	| tc == typeNatSubTyCon = do
		ea <- typeToExpression a
		eb <- typeToExpression b
		return $ ea .- eb
typeToExpression t = Left $ "typeToExpression: fail: " ++ showSDocUnsafe (ppr t)
