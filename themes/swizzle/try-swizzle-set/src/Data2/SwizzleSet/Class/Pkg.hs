{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data2.SwizzleSet.Class.Pkg where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Data2.SwizzleSet.Class.Base ()

swizzleClassPkg :: String
swizzleClassPkg = $(litE =<< stringL . (\(Module (PkgName pn) _) -> pn) . head
	. filter (\(Module _ mn) -> mn == ModName "Data2.SwizzleSet.Class.Base")
	. (\(ModuleInfo ms) -> ms) <$> (reifyModule =<< thisModule))
