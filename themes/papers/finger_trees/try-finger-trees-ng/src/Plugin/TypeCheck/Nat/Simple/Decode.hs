{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Plugin.TypeCheck.Nat.Simple.Decode (decode, Message(..)) where

import GhcPlugins (
	Var, promotedFalseDataCon, promotedTrueDataCon,
	Outputable(..), showSDocUnsafe, text )
import TyCoRep (Type(..), TyLit(..))
import TcTypeNats (typeNatLeqTyCon, typeNatAddTyCon, typeNatSubTyCon)
import Data.String (IsString(..))

import Data.Derivation.Expression (Exp(..), Number)

---------------------------------------------------------------------------

newtype Message = Message String deriving Show

decode :: Type -> Type -> Either Message (Exp Var Bool)
decode = typeToExpEq

typeToExpEq :: Type -> Type -> Either Message (Exp Var Bool)
typeToExpEq (TyVarTy v) tp2 =
	((Var v :==) <$> typeToExpVar tp2) <>
	((Var v :==) <$> typeToExpTerm tp2) <>
	((Var v :==) <$> typeToExpBool tp2)
typeToExpEq tp1 tp2 = case typeToExpTerm tp1 of
	Right tm1 -> (tm1 :==) <$> typeToExpTerm tp2
	Left _ -> case typeToExpBool tp1 of
		Right b1 -> (b1 :==) <$> typeToExpBool tp2
		Left em -> Left em

typeToExpVar :: Type -> Either Message (Exp Var a)
typeToExpVar (TyVarTy v) = Right $ Var v
typeToExpVar _ = Left $ Message "typeToExpVar: fail"

typeToExpTerm :: Type -> Either Message (Exp Var Number)
typeToExpTerm (TyVarTy v) = Right $ Var v
typeToExpTerm (LitTy (NumTyLit n)) = Right $ Const n
typeToExpTerm (TyConApp tc [a, b])
	| tc == typeNatAddTyCon =
		(:+) <$> typeToExpTerm a <*> typeToExpTerm b
	| tc == typeNatSubTyCon =
		(:-) <$> typeToExpTerm a <*> typeToExpTerm b
typeToExpTerm t = Left $ "typeToExpTerm: fail: " <> Message (showSDocUnsafe $ ppr t)

typeToExpBool :: Type -> Either Message (Exp Var Bool)
typeToExpBool (TyVarTy v) = Right $ Var v
typeToExpBool (TyConApp tc [])
	| tc == promotedFalseDataCon = Right $ Bool False
	| tc == promotedTrueDataCon = Right $ Bool True
typeToExpBool (TyConApp tc [t1, t2])
	| tc == typeNatLeqTyCon =
		(:<=) <$> typeToExpTerm t1 <*> typeToExpTerm t2
typeToExpBool t = Left $ "typeToExpBool: fail: " <> Message (showSDocUnsafe $ ppr t)

instance Semigroup Message where Message l <> Message r = Message $ l ++ r
instance IsString Message where fromString = Message
instance Outputable Message where ppr (Message msg) = text msg
