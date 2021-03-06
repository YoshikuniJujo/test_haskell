{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Plugin.TypeCheck.Nat.Simple.Decode where

import GhcPlugins (Var, promotedFalseDataCon, promotedTrueDataCon)
import TyCoRep
import TcTypeNats
import Outputable (Outputable(..), SDoc, (<+>), text)
import Control.Applicative ((<|>))
import Control.Monad.Trans.Except
import Data.String

import Data.Derivation.Expression

decode :: Type -> Type -> Except Message (Exp Var Bool)
decode (TyVarTy l) r = le <$> exVar r <|> le <$> exNum r <|> le <$> exBool r
	where le = (Var l :==)
decode l r = (:==) <$> exNum l <*> exNum r <|> (:==) <$> exBool l <*> exBool r

exBool :: Type -> Except Message (Exp Var Bool)
exBool (TyVarTy v) = pure $ Var v
exBool (TyConApp tc [])
	| tc == promotedFalseDataCon = pure $ Bool False
	| tc == promotedTrueDataCon = pure $ Bool True
exBool (TyConApp tc [l, r])
	| tc == typeNatLeqTyCon = (:<=) <$> exNum l <*> exNum r
exBool _ = throwE "exBool: fail"

exNum :: Type -> Except Message (Exp Var Number)
exNum (TyVarTy v) = pure $ Var v
exNum (LitTy (NumTyLit n)) = pure $ Const n
exNum (TyConApp tc [l, r])
	| tc == typeNatAddTyCon = (:+) <$> exNum l <*> exNum r
	| tc == typeNatSubTyCon = (:-) <$> exNum l <*> exNum r
exNum _ = throwE "exNum: fail"

exVar :: Type -> Except Message (Exp Var a)
exVar = \case TyVarTy v -> pure $ Var v; _ -> throwE "exVar: fail"

newtype Message = Message SDoc
instance Semigroup Message where Message l <> Message r = Message $ l <+> r
instance Monoid Message where mempty = Message ""
instance IsString Message where fromString = Message . text
instance Outputable Message where ppr (Message msg) = msg
