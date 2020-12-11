{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Plugin.TypeCheck.Nat.Simple.Decode where

import GhcPlugins (Var)
import TyCoRep
import Outputable (Outputable(..), SDoc, (<+>), text)
import Control.Monad.Trans.Except
import Data.String

import Data.Derivation.Expression

exVar :: Type -> Except Message (Exp Var a)
exVar = \case TyVarTy v -> pure $ Var v; _ -> throwE "exVar: fail"

newtype Message = Message SDoc
instance Semigroup Message where Message l <> Message r = Message $ l <+> r
instance Monoid Message where mempty = Message ""
instance IsString Message where fromString = Message . text
instance Outputable Message where ppr (Message msg) = msg
