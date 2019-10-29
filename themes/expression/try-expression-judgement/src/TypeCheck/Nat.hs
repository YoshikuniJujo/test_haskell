{-# LANGUAGE OverloadedStrings, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TypeCheck.Nat where

import Control.Arrow (second)
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

import Equal
import Geq

import qualified Derive as D

import Bool

plugin :: Plugin
plugin = defaultPlugin { tcPlugin = const . Just $ TcPlugin {
	tcPluginInit = return (),
	tcPluginSolve = const solveNat,
	tcPluginStop = const $ return () } }

solveNat :: [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
solveNat gs ds ws = do
	tcPluginTrace "!TypeCheck.Nat:" ""
	tcPluginTrace "!Given:" $ ppr gs
	tcPluginTrace "!Derives:" $ ppr ds
	tcPluginTrace "!Wanted:" $ ppr ws
	(tcPluginTrace "!Types" . ppr) `mapM_` rights (getTypes <$> ws)
	(tcPluginTrace "!Types 1 detail" . pprTypeDetail . fst) `mapM_` rights (getTypes <$> ws)
	(tcPluginTrace "!Types 2 detail" . pprTypeDetail . snd) `mapM_` rights (getTypes <$> ws)
	(tcPluginTrace "!Types" . ppr) `mapM_` (wantedExpression <$> ws)
	(tcPluginTrace "!Geq" . ppr) `mapM_` rights (ctToGeq <$> ws)
	(tcPluginTrace "Given detail 1" . pprTypeDetail . fst) `mapM_` rights (getTypes <$> gs)
	(tcPluginTrace "Given detail 2" . pprTypeDetail . snd) `mapM_` rights (getTypes <$> gs)
--	(tcPluginTrace "!VarBool" . ppr) `mapM_` rights (((uncurry typesToVarBool =<<) . getTypes) <$> gs)
--	(tcPluginTrace "!LeqVar" . ppr) `mapM_` rights (((uncurry typesToLeqVar =<<) . getTypes) <$> gs)
--	(tcPluginTrace "!VarBool or LeqVar" . ppr) `mapM_` rights (((uncurry varBoolOrLeqVar =<<) . getTypes) <$> gs)
--	tcPluginTrace "!VarBool or LeqVar" . ppr . partitionEithers $ rights (((uncurry varBoolOrLeqVar =<<) . getTypes) <$> gs)
	tcPluginTrace "Given Geqs" . ppr $ makeGeqsFromGivens gs
	tcPluginTrace "!D.Given" . ppr $ makeGiven gs
	let	geqs = rights (either Left (Right . D.WantedGeq) . ctToGeq <$> ws)
		gvns = makeGiven gs
	tcPluginTrace "!deriveFrom" . ppr $ D.deriveFrom gvns <$> geqs
	(tcPluginTrace "!Expression Given" . ppr) (Given.Given $ rights (expression <$> gs))
	(tcPluginTrace "!Expression Wanted" . ppr . Given.Wanted) `mapM_` rights (expression <$> ws)
	(tcPluginTrace "!Expression Wanted" . ppr) `mapM_` (expression <$> ws)
	(tcPluginTrace "!CanDerive" . ppr) `mapM_` (canDeriveCt gs <$> ws)
	let	oks = rights $ rights (canDeriveCt gs <$> ws)
		oks2 = rights $ rights (canDeriveCt2 gs <$> ws)
	return $ TcPluginOk (oks ++ oks2) []

canDeriveCt :: [Ct] -> Ct -> Either T.Text (Either Ct (EvTerm, Ct))
canDeriveCt gs_ w_ = either (Left . T.pack) Right $ do
	(t1, t2) <- getTypes w_
	let	gs = givenExpression gs_
	w <- wantedExpression w_
	b <- canDerive gs w
	if b then return $ Right (makeEvTerm t1 t2, w_) else return $ Left w_

canDeriveCt2 :: [Ct] -> Ct -> Either T.Text (Either Ct (EvTerm, Ct))
canDeriveCt2 gs_ w_ = either (Left . T.pack) Right $ do
	(t1, t2) <- getTypes w_
	w <- either Left (Right . D.WantedGeq) $ ctToGeq w_
	return $ if D.canDerive gs w then Right (makeEvTerm t1 t2, w_) else Left w_
	where gs = makeGiven gs_

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
	TyVarTy v -> "TyVarTy" <+> ppr v
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

typeToBool :: Type -> Either String Bool
typeToBool (TyConApp tc [])
	| tc == promotedFalseDataCon = Right False
	| tc == promotedTrueDataCon = Right True
typeToBool _ = Left "Not Promoted Bool"

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

equal :: Ct -> Either String (Equal Integer Var)
equal ct = do
	(t1, t2) <- getTypes ct
	e1 <- typeToExpression t1
	e2 <- typeToExpression t2
	return $ e1 .= e2

equalOrGeq :: Ct -> Either String (Either (Equal Integer Var) (Geq Integer Var))
equalOrGeq ct = case equal ct of
	Left _ -> Right <$> ctToGeq ct
	Right e -> return $ Left e

makeGiven :: [Ct] -> D.Given Integer Var
makeGiven gs = let
	egs = rights $ equalOrGeq <$> gs in
	uncurry D.Given . ((++ makeGeqsFromGivens gs) `second`) $ partitionEithers egs

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

typesToVarBool :: Type -> Type -> Either String (VarBool Var)
typesToVarBool (TyVarTy v1) (TyVarTy v2) = Right $ VarVar v1 v2
typesToVarBool (TyVarTy v1) t2 = do
	b <- typeToBool t2
	return $ VarBool v1 b
typesToVarBool t1 (TyVarTy v2) = do
	b <- typeToBool t1
	return $ VarBool v2 b
typesToVarBool _ _ = Left "not VarBool"

typesToLeqVar :: Type -> Type -> Either String (LeqVar Integer Var)
typesToLeqVar (TyVarTy v1) t2 = do
	(e1, e2) <- getLeq t2
	return $ LeqVar e1 e2 v1
typesToLeqVar t1 (TyVarTy v2) = do
	(e1, e2) <- getLeq t1
	return $ LeqVar e1 e2 v2
typesToLeqVar _ _ = Left "typesToLeqVar: not Leq"

varBoolOrLeqVar :: Type -> Type -> Either String (Either (VarBool Var) (LeqVar Integer Var))
varBoolOrLeqVar t1 t2 = case typesToVarBool t1 t2 of
	Left _ -> Right <$> typesToLeqVar t1 t2
	Right vb -> return $ Left vb


makeGeqsFromGivens :: [Ct] -> [Geq Integer Var]
makeGeqsFromGivens gs = rights . uncurry (map . mkGeq)
	. partitionEithers $ rights (((uncurry varBoolOrLeqVar =<<) . getTypes) <$> gs)
