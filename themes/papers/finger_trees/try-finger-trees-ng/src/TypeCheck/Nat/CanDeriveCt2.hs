{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TypeCheck.Nat.CanDeriveCt2 (canDeriveCt2) where

import Control.Arrow (second)
import Data.Bool
import Data.Either

import GhcPlugins

import TcRnTypes

import TyCoRep
import TcTypeNats
import TcEvidence

import TypeCheck.Nat.Orphans ()

import Equal
import Geq

import qualified Derive as D

import Bool

import TypeCheck.Nat.Common

import Expression

canDeriveCt2 :: [Ct] -> Ct -> Either String (EvTerm, Ct)
canDeriveCt2 gs w = do
	(t1, t2) <- getTypes w
	bool (Left "foo") (pure (makeEvTerm t1 t2, w)) . D.canDerive given
		=<< either Left (Right . D.WantedGeq) (ctToGeq w)
	where
	given = let
		egs = rights $ equalOrGeq <$> gs in
		uncurry D.Given . ((++ makeGeqsFromGivens) `second`) $ partitionEithers egs
	makeGeqsFromGivens = rights . uncurry (map . mkGeq)
		. partitionEithers $ rights (((uncurry varBoolOrLeqVar =<<) . getTypes) <$> gs)

varBoolOrLeqVar :: Type -> Type -> Either String (Either (VarBool Var) (LeqVar Integer Var))
varBoolOrLeqVar t1 t2 = case typesToVarBool t1 t2 of
	Left _ -> Right <$> typesToLeqVar
	Right vb -> return $ Left vb
	where typesToLeqVar = case (t1, t2) of
		(TyVarTy v1, _t2) -> do
			(e1, e2) <- getLeq t2
			return $ LeqVar e1 e2 v1
		(_t1, TyVarTy v2) -> do
			(e1, e2) <- getLeq t1
			return $ LeqVar e1 e2 v2
		_ -> Left "typesToLeqVar: not Leq"

typesToVarBool :: Type -> Type -> Either String (VarBool Var)
typesToVarBool (TyVarTy v1) (TyVarTy v2) = Right $ VarVar v1 v2
typesToVarBool (TyVarTy v1) t2 = do
	b <- typeToBool t2
	return $ VarBool v1 b
typesToVarBool t1 (TyVarTy v2) = do
	b <- typeToBool t1
	return $ VarBool v2 b
typesToVarBool _ _ = Left ("not VarBool" :: String)

typeToBool :: Type -> Either String Bool
typeToBool (TyConApp tc [])
	| tc == promotedFalseDataCon = Right False
	| tc == promotedTrueDataCon = Right True
typeToBool _ = Left "Not Promoted Bool"

equalOrGeq :: Ct -> Either String (Either (Equal Integer Var) (Geq Integer Var))
equalOrGeq ct = case eq of
	Left _ -> Right <$> ctToGeq ct
	Right e -> return $ Left e
	where
	eq = do
		(t1, t2) <- getTypes ct
		e1 <- typeToExpression t1
		e2 <- typeToExpression t2
		return $ e1 .= e2

ctToGeq :: Ct -> Either String (Geq Integer Var)
ctToGeq ct = uncurry typesToGeq =<< getTypes ct

typesToGeq :: Type -> Type -> Either String (Geq Integer Var)
typesToGeq t1 t2 | isTrue t2 = do
	(e1, e2) <- getLeq t1
	return $ e2 .>= e1
	where
	isTrue (TyConApp tc []) = tc == promotedTrueDataCon
	isTrue _ = False
typesToGeq _ _ = Left "typesToGeq: not true"
	
getLeq :: Type -> Either String (Expression Integer Var, Expression Integer Var)
getLeq (TyConApp tc [t1, t2])
	| tc == typeNatLeqTyCon = (,) <$> typeToExpression t1 <*> typeToExpression t2
getLeq _ = Left "getLeq: not Leq"
