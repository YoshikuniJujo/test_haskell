{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Foo (foo) where

import Language.Haskell.TH

foo :: Integer -> ExpQ
foo n = infixE
	(Just . litE $ integerL n)
	(varE '(+))
	(Just . litE $ integerL 3)
