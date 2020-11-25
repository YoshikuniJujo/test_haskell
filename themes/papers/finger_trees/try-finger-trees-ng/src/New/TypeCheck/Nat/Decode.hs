{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module New.TypeCheck.Nat.Decode where

import GhcPlugins hiding (Expr(Var), (<>))
import TcRnTypes
import TcTypeNats
import TyCoRep

import New.Expression

decode :: Ct -> Either String (Exp Var Bool, Bool)
decode = undefined

unNomEq :: Ct -> Either String (Type, Type)
unNomEq ct = case classifyPredType . ctEvPred $ ctEvidence ct of
	EqPred NomEq t1 t2 -> Right (t1, t2)
	_ -> Left "Cannot unNomEq"

{-
typeToExpEq :: Type -> Type -> Either String (Exp Var Bool)
typeToExpEq tp1 tp2 = case typeToExpVar tp1 of
	Right v1 -> ((v1 :==) <$> typeToExpVar tp2) <>
		((v1 :==) <$> typeToExpTerm tp2) <>
		((v1 :==) <$> typeToExpBool tp2)
	Left _ -> case typeToExpTerm tp1 of
		Right tm1 -> (tm1 :==) <$> typeToExpTerm tp2
		Left _ -> case typeToExpBool tp1 of
			Right tb1 -> (tb1 :==) <$> typeToExpBool tp2
			Left _ -> Left "typeToExpEq: fail"
			-}

typeToExpVar :: Type -> Either String (Exp Var a)
typeToExpVar (TyVarTy v) = Right $ Var v
typeToExpVar _ = Left "typeToExpVar: fail"

typeToExpTerm :: Type -> Either String (Exp Var Term)
typeToExpTerm (TyVarTy v) = Right $ Var v
typeToExpTerm (LitTy (NumTyLit n)) = Right $ Const n
typeToExpTerm (TyConApp tc [a, b])
	| tc == typeNatAddTyCon =
		(:+) <$> typeToExpTerm a <*> typeToExpTerm b
	| tc == typeNatSubTyCon =
		(:-) <$> typeToExpTerm a <*> typeToExpTerm b
typeToExpTerm _ = Left "typeToExpTerm: fail"

typeToExpBool :: Type -> Either String (Exp Var Bool)
typeToExpBool (TyVarTy v) = Right $ Var v
typeToExpBool (TyConApp tc [])
	| tc == promotedFalseDataCon = Right $ Bool False
	| tc == promotedTrueDataCon = Right $ Bool True
typeToExpBool (TyConApp tc [t1, t2])
	| tc == typeNatLeqTyCon =
		(:<=) <$> typeToExpTerm t1 <*> typeToExpTerm t2
typeToExpBool _ = Left "typeToExpBool: fail"
