{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.SwizzleLens.Pkg where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Data.Swizzle.Class ()
import Data.SwizzleSet.Class ()

swizzleClassPkg :: String
swizzleClassPkg = $(litE =<< stringL . (\(Module (PkgName pn) _) -> pn) . head
	. filter (\(Module _ mn) -> mn == ModName "Data.Swizzle.Class")
	. (\(ModuleInfo ms) -> ms) <$> (reifyModule =<< thisModule))

swizzleSetClassPkg :: String
swizzleSetClassPkg = $(litE =<< stringL . (\(Module (PkgName pn) _) -> pn) . head
	. filter (\(Module _ mn) -> mn == ModName "Data.SwizzleSet.Class")
	. (\(ModuleInfo ms) -> ms) <$> (reifyModule =<< thisModule))
