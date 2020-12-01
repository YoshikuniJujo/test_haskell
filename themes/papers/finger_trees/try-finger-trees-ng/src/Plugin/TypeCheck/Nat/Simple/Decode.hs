{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Plugin.TypeCheck.Nat.Simple.Decode where

import GhcPlugins hiding (Expr(Var), (<>))
import TcRnTypes
import TcTypeNats
import TyCoRep

import Outputable hiding ((<>))

import Data.Derivation.Expression

import qualified Data.Text as T

decode :: Ct -> Either T.Text (Exp Var Bool)
decode = ctToExpEq

ctToExpEq :: Ct -> Either T.Text (Exp Var Bool)
ctToExpEq ct = do
	(t1, t2) <- unNomEq ct
	typeToExpEq t1 t2

unNomEq :: Ct -> Either T.Text (Type, Type)
unNomEq ct = case classifyPredType . ctEvPred $ ctEvidence ct of
	EqPred NomEq t1 t2 -> Right (t1, t2)
	EqPred foo t1 t2 -> Left . T.pack $
		showSDocUnsafe (ppr foo) ++ " " ++
		showSDocUnsafe (ppr t1) ++ " " ++
		showSDocUnsafe (ppr t2) ++ " Cannot unNOmEq"
	_ -> Left $ T.pack "Cannot unNomEq"

typeToExpEq :: Type -> Type -> Either T.Text (Exp Var Bool)
typeToExpEq (TyVarTy v) tp2 =
	((Var v :==) <$> typeToExpVar tp2) <>
	((Var v :==) <$> typeToExpTerm tp2) <>
	((Var v :==) <$> typeToExpBool tp2)
typeToExpEq tp1 tp2 = case typeToExpTerm tp1 of
	Right tm1 -> (tm1 :==) <$> typeToExpTerm tp2
	Left _ -> case typeToExpBool tp1 of
		Right b1 -> (b1 :==) <$> typeToExpBool tp2
		Left em -> Left em

typeToExpVar :: Type -> Either T.Text (Exp Var a)
typeToExpVar (TyVarTy v) = Right $ Var v
typeToExpVar _ = Left $ T.pack "typeToExpVar: fail"

typeToExpTerm :: Type -> Either T.Text (Exp Var Term)
typeToExpTerm (TyVarTy v) = Right $ Var v
typeToExpTerm (LitTy (NumTyLit n)) = Right $ Const n
typeToExpTerm (TyConApp tc [a, b])
	| tc == typeNatAddTyCon =
		(:+) <$> typeToExpTerm a <*> typeToExpTerm b
	| tc == typeNatSubTyCon =
		(:-) <$> typeToExpTerm a <*> typeToExpTerm b
typeToExpTerm t = Left . T.pack $ "typeToExpTerm: fail: " ++ showSDocUnsafe (ppr t)

typeToExpBool :: Type -> Either T.Text (Exp Var Bool)
typeToExpBool (TyVarTy v) = Right $ Var v
typeToExpBool (TyConApp tc [])
	| tc == promotedFalseDataCon = Right $ Bool False
	| tc == promotedTrueDataCon = Right $ Bool True
typeToExpBool (TyConApp tc [t1, t2])
	| tc == typeNatLeqTyCon =
		(:<=) <$> typeToExpTerm t1 <*> typeToExpTerm t2
typeToExpBool t = Left . T.pack $ "typeToExpBool: fail: " ++ showSDocUnsafe (ppr t)

instance Outputable T.Text where
	ppr = text . show
