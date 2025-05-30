{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.SwizzleModify.Pkg where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Data.SwizzleModify.Base ()

swizzleModifyPkg :: String
swizzleModifyPkg = $(litE =<< stringL . (\(Module (PkgName pn) _) -> pn) . head
	. filter (\(Module _ mn) -> mn == ModName "Data.SwizzleModify.Base")
	. (\(ModuleInfo ms) -> ms) <$> (reifyModule =<< thisModule))
