{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Bool where

import Prelude hiding ((<>))

import Data.Bool

import Outputable

import Expression
import Geq

data LeqVar i v = LeqVar (Expression i v) (Expression i v) v deriving Show
data VarBool v = VarVar v v | VarBool v Bool deriving Show

instance (Outputable i, Outputable v) => Outputable (LeqVar i v) where
	ppr (LeqVar e1 e2 v) = "(LeqVar" <+> ppr e1 <+> ppr e2 <+> ppr v <> ")"

instance Outputable v => Outputable (VarBool v) where
	ppr (VarVar v1 v2) = "(VarVar" <+> ppr v1 <+> ppr v2 <> ")"
	ppr (VarBool v b) = "(VarBool" <+> ppr v <+> ppr b <> ")"

data Great i v = Great (Expression i v) deriving (Show, Eq)

infix 4 .>

(.>) :: (Integral i, Ord v) => Expression i v -> Expression i v -> Great i v
e1 .> e2 = Great . reduct $ e1 .- e2

checkBool :: Eq v => [VarBool v] -> [VarBool v] -> v -> Either String Bool
checkBool _ [] _ = Left "checkBool: error"
checkBool va (VarVar v1 v2 : _) v0 | v1 == v0 = checkBool va va v2
checkBool _ (VarBool v1 b : _) v0 | v1 == v0 = Right b
checkBool va (_ : vs) v0 = checkBool va vs v0

mkGreatGeq :: (Integral i, Ord v) => [VarBool v] -> LeqVar i v -> Either String (Either (Great i v) (Geq i v))
mkGreatGeq vs (LeqVar e1 e2 v) =
	bool (Left $ e1 .> e2) (Right $ e2 .>= e1) <$> checkBool vs vs v

mkGeq :: (Integral i, Ord v) => [VarBool v] -> LeqVar i v -> Either String (Geq i v)
mkGeq vs lv = do
	egg <- mkGreatGeq vs lv
	either (const $ Left "mkGeq: It's Great") return egg

-- Equal
-- Geq
-- VarBool
-- LeqVar
