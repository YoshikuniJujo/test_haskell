{-# LANGUAGE TemplateHaskell #-}

module Sandbox (
	foo
	) where

import Language.Haskell.TH

foo = $(litE =<< stringL  <$> runIO (readFile "data/foo.txt"))
