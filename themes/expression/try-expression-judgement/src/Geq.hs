{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Geq where

import Prelude hiding ((<>))

import Outputable

import Expression

data Geq i v = Eq (Expression i v) | Geq (Expression i v) deriving Show

instance (Outputable i, Outputable v) => Outputable (Geq i v) where
	ppr (Eq e) = "(Eq " <+> ppr e <> ")"
	ppr (Geq e) = "(Geq " <+> ppr e <> ")"

newtype GivenGeq i v = GivenGeq [Geq i v] deriving (Show, Outputable)
