{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TypeCheck.Nat.Common (getTypes, makeEvTerm, typeToExpression) where

import GhcPlugins

import TcRnTypes

import TyCoRep
import TcTypeNats
import TcEvidence

import Expression
import TypeCheck.Nat.Orphans ()

getTypes :: Ct -> Either String (Type, Type)
getTypes ct = case classifyPredType . ctEvPred $ ctEvidence ct of
	EqPred NomEq t1 t2 -> Right (t1, t2)
	EqPred foo t1 t2 -> Left $ showSDocUnsafe (ppr foo) ++ " " ++ showSDocUnsafe (ppr t1) ++ " " ++ showSDocUnsafe (ppr t2) ++ "Cannot get types"
	_ -> Left "Cannot get types"

makeEvTerm :: Type -> Type -> EvTerm
makeEvTerm t1 t2 = EvExpr . Coercion
	$ mkUnivCo (PluginProv "TypeCheck.Nat") Nominal t1 t2

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
