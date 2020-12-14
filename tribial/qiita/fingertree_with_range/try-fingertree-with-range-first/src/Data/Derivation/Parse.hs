{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Derivation.Parse where

import Control.Applicative
import Control.Arrow
import Data.Function
import Data.Maybe
import Data.List

import Data.Parse
import Data.Derivation.Expression

import qualified Data.Bool as B

data Memo = Memo {
	constraint :: Maybe (Exp Var Bool, Memo),
	equal :: Maybe (Exp Var Bool, Memo),
	bool :: Maybe (Exp Var Bool, Memo),
	lessEqual :: Maybe (Exp Var Bool, Memo),
	polynomial :: Maybe (Exp Var Number, Memo),
	number :: Maybe (Exp Var Number, Memo),
	token :: Maybe (String, Memo) }

type Var = String
