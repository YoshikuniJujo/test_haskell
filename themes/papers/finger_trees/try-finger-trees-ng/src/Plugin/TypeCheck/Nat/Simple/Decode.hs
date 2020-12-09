{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
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
decode (TyVarTy l) r = le <$> exVar r <|> le <$> exNum r <|> le <$> exBool r
	where le = (Var l :==)
decode l r = (:==) <$> exNum l <*> exNum r <|> (:==) <$> exBool l <*> exBool r

exBool :: Type -> Except Message (Exp Var Bool)
exBool (TyVarTy v) = pure $ Var v
exBool (TyConApp tc [])
	| tc == promotedFalseDataCon = pure $ Bool False
	| tc == promotedTrueDataCon = pure $ Bool True
exBool (TyConApp tc [t1, t2])
	| tc == typeNatLeqTyCon = (:<=) <$> exNum t1 <*> exNum t2
exBool t = throwE $ "exBool: fail: " <> Message (showSDocUnsafe $ ppr t)

exNum :: Type -> Except Message (Exp Var Number)
exNum (TyVarTy v) = pure $ Var v
exNum (LitTy (NumTyLit n)) = pure $ Const n
exNum (TyConApp tc [a, b])
	| tc == typeNatAddTyCon = (:+) <$> exNum a <*> exNum b
	| tc == typeNatSubTyCon = (:-) <$> exNum a <*> exNum b
exNum t = throwE $ "exNum: fail: " <> Message (showSDocUnsafe $ ppr t)

exVar :: Type -> Except Message (Exp Var a)
exVar = \case TyVarTy v -> pure $ Var v; _ -> throwE $ Message "exVar: fail"

---------------------------------------------------------------------------
-- MESSAGE
---------------------------------------------------------------------------

newtype Message = Message String deriving Show
instance Semigroup Message where Message l <> Message r = Message $ l ++ r
instance Monoid Message where mempty = Message ""
instance IsString Message where fromString = Message
instance Outputable Message where ppr (Message msg) = text msg
