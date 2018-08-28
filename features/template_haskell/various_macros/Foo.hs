{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

{-# LANGUAGE TemplateHaskell #-}

module Foo (foo, foo') where

import Language.Haskell.TH

foo :: Integer -> ExpQ
foo n = infixE
	(Just . litE $ integerL n)
	(varE '(+))
	(Just . litE $ integerL 3)

foo' :: Integer -> ExpQ
foo' n = uInfixE (litE $ integerL n) (varE '(+)) (litE $ integerL 3)
