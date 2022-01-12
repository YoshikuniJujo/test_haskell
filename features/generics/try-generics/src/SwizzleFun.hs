{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module SwizzleFun where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import SwizzleClassPkg

import qualified SwizzleClass as S

funY :: ExpQ
funY = varE $ mkNameG_v swizzleClassPkg "SwizzleClass" "y"
