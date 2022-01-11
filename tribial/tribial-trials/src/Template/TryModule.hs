{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Template.TryModule where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Template.ModuleTree
import Template.Foo

do	m@(Module pn mn) <- thisModule
	runIO . print $ ppr m
	runIO $ print pn
	runIO $ print mn

	runIO . print . (ppr <$>) . (\(ModuleInfo ms) -> ms) =<< reifyModule m
	pure []

do	runIO . print =<< moduleTree 3 =<< thisModule
	pure []
