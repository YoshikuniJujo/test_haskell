{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
-- {-# OPTIONS_GHC -Wall -fno-warn-tabs -this-unit-id "tribial-trials" #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Template.NameSpace where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Maybe

import Template.Foo (foo)

import Data.Version
import Paths_tribial_trials

-- foo :: Int
-- foo = 321

fooTh, fooTh', fooTh'', fooTh''', fooTh4, fooTh5, fooTh6 :: ExpQ
fooTh = varE $ mkName "foo"
fooTh' = varE $ mkName "Template.Foo.foo"
fooTh'' = varE 'foo

fooTh''' = varE . fromJust =<<  lookupValueName "foo"


-- fooTh4 = varE $ mkNameG_v ("tribial-trials-" ++ showVersion version) "Template.Foo" "foo"
fooTh4 = do
	runIO . print . (ppr <$>) . (\(ModuleInfo ms) -> ms) =<< reifyModule =<< thisModule
	PkgName pn <- (\(Module pn _) -> pn) . head
		. filter (\(Module _ mn) -> mn == ModName "Template.Foo")
		. (\(ModuleInfo ms) -> ms) <$> (reifyModule =<< thisModule)
	runIO $ print pn
	varE $ mkNameG_v pn "Template.Foo" "foo"

fooTh5 = do
	PkgName pn <- (\(Module pn _) -> pn) . head
		. filter (\(Module _ mn) -> mn == ModName "Template.Foo")
		. (\(ModuleInfo ms) -> ms) <$> (reifyModule =<< thisModule)
	varE $ mkNameG_v pn "Template.Foo" "foo"

fooTh6 = let pn = $(litE . stringL =<< do
		PkgName pn <- (\(Module pn _) -> pn) . head
			. filter (\(Module _ mn) -> mn == ModName "Template.Foo")
			. (\(ModuleInfo ms) -> ms) <$> (reifyModule =<< thisModule)
		pure pn)
		in
	varE $ mkNameG_v pn "Template.Foo" "foo"
