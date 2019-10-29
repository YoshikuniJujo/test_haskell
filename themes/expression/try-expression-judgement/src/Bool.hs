{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Bool where

import Data.Bool

import Expression
import Geq

data LeqVar i v = LeqVar (Expression i v) (Expression i v) v deriving Show
data VarBool v = VarVar v v | VarBool v Bool deriving Show

data Great i v = Great (Expression i v) deriving (Show, Eq)

infix 4 .>

(.>) :: (Integral i, Ord v) => Expression i v -> Expression i v -> Great i v
e1 .> e2 = Great . reduct $ e1 .- e2

checkBool :: Eq v => [VarBool v] -> [VarBool v] -> v -> Maybe Bool
checkBool _ [] _ = Nothing
checkBool va (VarVar v1 v2 : _) v0 | v1 == v0 = checkBool va va v2
checkBool _ (VarBool v1 b : _) v0 | v1 == v0 = Just b
checkBool va (_ : vs) v0 = checkBool va vs v0

mkGreatGeq :: (Integral i, Ord v) => [VarBool v] -> LeqVar i v -> Maybe (Either (Great i v) (Geq i v))
mkGreatGeq vs (LeqVar e1 e2 v) =
	bool (Left $ e1 .> e2) (Right $ e2 .>= e1) <$> checkBool vs vs v

mkGeq :: (Integral i, Ord v) => [VarBool v] -> LeqVar i v -> Maybe (Geq i v)
mkGeq vs lv = do
	egg <- mkGreatGeq vs lv
	either (const Nothing) return egg

-- Equal
-- Geq
-- VarBool
-- LeqVar
