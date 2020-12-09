{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Plugin.TypeCheck.Nat.Simple.Decode (decode, Message(..)) where

import GhcPlugins (
	Var, promotedFalseDataCon, promotedTrueDataCon,
	Outputable(..), showSDocUnsafe, text )
import TyCoRep (Type(..), TyLit(..))
import TcTypeNats (typeNatLeqTyCon, typeNatAddTyCon, typeNatSubTyCon)
import Control.Applicative ((<|>))
import Control.Monad.Trans.Except
import Data.String (IsString(..))

import Data.Derivation.Expression (Exp(..), Number)

---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- DECODE
---------------------------------------------------------------------------

decode :: Type -> Type -> Either Message (Exp Var Bool)
decode t1 t2 = runExcept $ decode' t1 t2

decode' :: Type -> Type -> Except Message (Exp Var Bool)
decode' (TyVarTy v) tp2 =
	veq <$> expVar tp2 <|> veq <$> expTerm tp2 <|> veq <$> expBool tp2
	where veq = (Var v :==)
decode' tp1 tp2 = do
		tm1 <- expTerm tp1
		(tm1 :==) <$> expTerm tp2
	`catchE` \_ -> do
		b1 <- expBool tp1
		(b1 :==) <$> expBool tp2

expVar :: Type -> Except Message (Exp Var a)
expVar (TyVarTy v) = pure $ Var v
expVar _ = throwE $ Message "expVar: fail"

expTerm :: Type -> Except Message (Exp Var Number)
expTerm (TyVarTy v) = pure $ Var v
expTerm (LitTy (NumTyLit n)) = pure $ Const n
expTerm (TyConApp tc [a, b])
	| tc == typeNatAddTyCon =
		(:+) <$> expTerm a <*> expTerm b
	| tc == typeNatSubTyCon =
		(:-) <$> expTerm a <*> expTerm b
expTerm t = throwE $ "expTerm: fail: " <> Message (showSDocUnsafe $ ppr t)

expBool :: Type -> Except Message (Exp Var Bool)
expBool (TyVarTy v) = pure $ Var v
expBool (TyConApp tc [])
	| tc == promotedFalseDataCon = pure $ Bool False
	| tc == promotedTrueDataCon = pure $ Bool True
expBool (TyConApp tc [t1, t2])
	| tc == typeNatLeqTyCon =
		(:<=) <$> expTerm t1 <*> expTerm t2
expBool t = throwE $ "expBool: fail: " <> Message (showSDocUnsafe $ ppr t)

---------------------------------------------------------------------------
-- MESSAGE
---------------------------------------------------------------------------

newtype Message = Message String deriving Show
instance Semigroup Message where Message l <> Message r = Message $ l ++ r
instance Monoid Message where mempty = Message ""
instance IsString Message where fromString = Message
instance Outputable Message where ppr (Message msg) = text msg
