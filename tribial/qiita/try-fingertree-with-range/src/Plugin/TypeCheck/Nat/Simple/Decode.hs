{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Plugin.TypeCheck.Nat.Simple.Decode where

import GHC.Core.TyCon
import GHC.Core.TyCo.Rep
import GHC.Tc.Plugin
import GHC.TcPluginM.Extra
import GHC.Types.Var
import GHC.Builtin.Types
import GHC.Builtin.Types.Literals
import GHC.Utils.Outputable (Outputable(..), SDoc, (<+>), text)
import Control.Applicative ((<|>))
import Control.Monad.Trans.Except
import Data.String
import Data.Derivation.Expression

import Data.Type.Ord
import GHC.Data.FastString
import GHC.Unit.Module
import GHC.Types.Name.Occurrence

newtype Message = Message SDoc

instance Semigroup Message where Message l <> Message r = Message $ l <+> r
instance Monoid Message where mempty = Message ""
instance IsString Message where fromString = Message . text
instance Outputable Message where ppr (Message msg) = msg

decode :: TyCon -> Type -> Type -> Except Message (Exp Var Bool)
decode oc (TyVarTy l) r = le <$> exVar r <|> le <$> exNum r <|> le <$> exBool oc r
	where le = (Var l :==)
decode oc l r = (:==) <$> exNum l <*> exNum r <|> (:==) <$> exBool oc l <*> exBool oc r

exVar :: Type -> Except Message (Exp Var a)
exVar = \case TyVarTy v -> pure $ Var v; _ -> throwE "exVar: fail"

exNum :: Type -> Except Message (Exp Var Number)
exNum (TyVarTy v) = pure $ Var v
exNum (LitTy (NumTyLit n)) = pure $ Const n
exNum (TyConApp tc [l, r])
	| tc == typeNatAddTyCon = (:+) <$> exNum l <*> exNum r
	| tc == typeNatSubTyCon = (:-) <$> exNum l <*> exNum r
exNum _ = throwE "exNum: fail"

exBool :: TyCon -> Type -> Except Message (Exp Var Bool)
exBool _ (TyVarTy v) = pure $ Var v
exBool _ (TyConApp tc [])
	| tc == promotedFalseDataCon = pure $ Bool False
	| tc == promotedTrueDataCon = pure $ Bool True
-- exBool (TyConApp tc [l, r])
--	| tc == typeNatLeqTyCon = (:<=) <$> exNum l <*> exNum r
exBool oc (TyConApp tc lr) = throwE $ "exBool: fail: TyConApp" <> Message (ppr oc) <> Message (ppr tc) <> Message (ppr lr)
exBool _ _ = throwE "exBool: fail"

lookupOrdCond :: TcPluginM TyCon
lookupOrdCond = do
	md2 <- lookupModule ordModule basePackage
	look md2 "OrdCond"
	where
	ordModule = mkModuleName "Data.Type.Ord"
	basePackage = fsLit "base"
	look md s = tcLookupTyCon =<< lookupName md (mkTcOcc s)
