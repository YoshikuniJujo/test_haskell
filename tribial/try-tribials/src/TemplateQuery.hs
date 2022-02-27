{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TemplateQuery where

import Language.Haskell.TH

data Foo = Foo

fooValueExist = $(
	maybe (litE $ StringL "Foo not exist") (const . litE $ StringL "Foo exist")
		=<< lookupValueName "Foo")

fooTypeExist = $(
	maybe (litE $ StringL "Foo not exist") (const . litE $ StringL "Foo exist")
		=<< lookupTypeName "Foo")
