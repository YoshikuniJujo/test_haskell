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

-- * DECODE
-- * MESSAGE

---------------------------------------------------------------------------
-- DECODE
---------------------------------------------------------------------------

decode :: Type -> Type -> Except Message (Exp Var Bool)
decode (TyVarTy l) r = le <$> exVar r <|> le <$> exTerm r <|> le <$> exBool r
	where le = (Var l :==)
decode l r = (:==) <$> exTerm l <*> exTerm r <|> (:==) <$> exBool l <*> exBool r

exVar :: Type -> Except Message (Exp Var a)
exVar (TyVarTy v) = pure $ Var v
exVar _ = throwE $ Message "exVar: fail"

exTerm :: Type -> Except Message (Exp Var Number)
exTerm (TyVarTy v) = pure $ Var v
exTerm (LitTy (NumTyLit n)) = pure $ Const n
exTerm (TyConApp tc [a, b])
	| tc == typeNatAddTyCon = (:+) <$> exTerm a <*> exTerm b
	| tc == typeNatSubTyCon = (:-) <$> exTerm a <*> exTerm b
exTerm t = throwE $ "exTerm: fail: " <> Message (showSDocUnsafe $ ppr t)

exBool :: Type -> Except Message (Exp Var Bool)
exBool (TyVarTy v) = pure $ Var v
exBool (TyConApp tc [])
	| tc == promotedFalseDataCon = pure $ Bool False
	| tc == promotedTrueDataCon = pure $ Bool True
exBool (TyConApp tc [t1, t2])
	| tc == typeNatLeqTyCon = (:<=) <$> exTerm t1 <*> exTerm t2
exBool t = throwE $ "exBool: fail: " <> Message (showSDocUnsafe $ ppr t)

---------------------------------------------------------------------------
-- MESSAGE
---------------------------------------------------------------------------

newtype Message = Message String deriving Show
instance Semigroup Message where Message l <> Message r = Message $ l ++ r
instance Monoid Message where mempty = Message ""
instance IsString Message where fromString = Message
instance Outputable Message where ppr (Message msg) = text msg
