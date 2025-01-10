{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Swizzle.Class.Pkg where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Data.Swizzle.Class.Base ()

swizzleClassPkg :: String
swizzleClassPkg = $(litE =<< stringL . (\(Module (PkgName pn) _) -> pn) . head
	. filter (\(Module _ mn) -> mn == ModName "Data.Swizzle.Class.Base")
	. (\(ModuleInfo ms) -> ms) <$> (reifyModule =<< thisModule))
