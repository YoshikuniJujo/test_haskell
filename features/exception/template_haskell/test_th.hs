{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH

sequence [
	valD (varP $ mkName "x") (normalB . litE $ integerL 888) []
	]
