{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module SwizzleClassPkg where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import SwizzleClass ()

swizzleClassPkg :: String
swizzleClassPkg = $(litE =<< stringL . (\(Module (PkgName pn) _) -> pn) . head
	. filter (\(Module _ mn) -> mn == ModName "SwizzleClass")
	. (\(ModuleInfo ms) -> ms) <$> (reifyModule =<< thisModule))
