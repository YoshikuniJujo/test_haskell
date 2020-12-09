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
decode = (runExcept .) . expEq

expEq :: Type -> Type -> Except Message (Exp Var Bool)
expEq (TyVarTy l) r = le <$> expVar r <|> le <$> expTerm r <|> le <$> expBool r
	where le = (Var l :==)
expEq l r =
	((:==) <$> expTerm l <*> expTerm r) `catchE` const
	((:==) <$> expBool l <*> expBool r)

expVar :: Type -> Except Message (Exp Var a)
expVar (TyVarTy v) = pure $ Var v
expVar _ = throwE $ Message "expVar: fail"

expTerm :: Type -> Except Message (Exp Var Number)
expTerm (TyVarTy v) = pure $ Var v
expTerm (LitTy (NumTyLit n)) = pure $ Const n
expTerm (TyConApp tc [a, b])
	| tc == typeNatAddTyCon = (:+) <$> expTerm a <*> expTerm b
	| tc == typeNatSubTyCon = (:-) <$> expTerm a <*> expTerm b
expTerm t = throwE $ "expTerm: fail: " <> Message (showSDocUnsafe $ ppr t)

expBool :: Type -> Except Message (Exp Var Bool)
expBool (TyVarTy v) = pure $ Var v
expBool (TyConApp tc [])
	| tc == promotedFalseDataCon = pure $ Bool False
	| tc == promotedTrueDataCon = pure $ Bool True
expBool (TyConApp tc [t1, t2])
	| tc == typeNatLeqTyCon = (:<=) <$> expTerm t1 <*> expTerm t2
expBool t = throwE $ "expBool: fail: " <> Message (showSDocUnsafe $ ppr t)

---------------------------------------------------------------------------
-- MESSAGE
---------------------------------------------------------------------------

newtype Message = Message String deriving Show
instance Semigroup Message where Message l <> Message r = Message $ l ++ r
instance Monoid Message where mempty = Message ""
instance IsString Message where fromString = Message
instance Outputable Message where ppr (Message msg) = text msg
