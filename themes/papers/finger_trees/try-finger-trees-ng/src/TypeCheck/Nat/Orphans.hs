{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-orphans #-}

module TypeCheck.Nat.Orphans () where

import Outputable
import Var

instance Show Var where show = showSDocUnsafe . ppr
