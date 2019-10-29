{-# LANGUAGE OverloadedStrings, LambdaCase #-}
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

import TyCon
import TysWiredIn

import Given
import Expression
import TypeCheck.Nat.Orphans ()

import Geq

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
	(tcPluginTrace "!Types 1 detail" . pprTypeDetail . fst) `mapM_` rights (getTypes <$> ws)
	(tcPluginTrace "!Types 2 detail" . pprTypeDetail . snd) `mapM_` rights (getTypes <$> ws)
	(tcPluginTrace "!Types" . ppr) `mapM_` (wantedExpression <$> ws)
	(tcPluginTrace "!Geq" . ppr) `mapM_` rights (ctToGeq <$> ws)
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
	EqPred NomEq t1 t2 -> Right (t1, t2)
	EqPred foo t1 t2 -> Left $ showSDocUnsafe (ppr foo) ++ " " ++ showSDocUnsafe (ppr t1) ++ " " ++ showSDocUnsafe (ppr t2) ++ "Cannot get types"
	_ -> Left "Cannot get types"

pprTypeDetail :: Type -> SDoc
pprTypeDetail = \case
	TyConApp tc ts -> case tc of
		_	| isAlgTyCon tc -> "TyConApp" <+> "(AlgTyCon" <+> ppr (tyConName tc) <+> ")" <+> ppr ts
			| isPrimTyCon tc -> "TyConApp" <+> "(PrimTyCon" <+> ppr (tyConName tc) <+> ")" <+> ppr ts
			| tc == promotedTrueDataCon -> "TyConApp" <+> "promotedTrueDataCon" <+> ppr tc <+> ")" <+> ppr ts
			| tc == typeNatLeqTyCon -> "TyConApp" <+> "typeNatLeqTyCon" <+> ppr (typeToExpression <$> ts)
			| otherwise -> "TyConApp" <+> ppr tc <+> ppr ts
	t -> ppr t

isTrue :: Type -> Bool
isTrue (TyConApp tc []) = tc == promotedTrueDataCon
isTrue _ = False

getLeq :: Type -> Either String (Expression Integer Var, Expression Integer Var)
getLeq (TyConApp tc [t1, t2])
	| tc == typeNatLeqTyCon = (,) <$> typeToExpression t1 <*> typeToExpression t2
getLeq _ = Left "getLeq: not Leq"

ctToGeq :: Ct -> Either String (Geq Integer Var)
ctToGeq ct = uncurry typesToGeq =<< getTypes ct

typesToGeq :: Type -> Type -> Either String (Geq Integer Var)
typesToGeq t1 t2 | isTrue t2 = do
	(e1, e2) <- getLeq t1
	return $ e2 .>= e1
typesToGeq _ _ = Left "typesToGeq: not true"

expression :: Ct -> Either String (Expression Integer Var)
expression ct = do
	(t1, t2) <- getTypes ct
	e1 <- typeToExpression t1
	e2 <- typeToExpression t2
	return . reductAndNormalizeSign $ e1 .- e2

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
