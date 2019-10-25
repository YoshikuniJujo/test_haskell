{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-orphans #-}

module TypeCheck.Nat.Orphans () where

import Outputable
import Var

import qualified Data.Text as T

instance Show Var where
	show = showSDocUnsafe . ppr

instance Outputable T.Text where
	ppr = text . show
