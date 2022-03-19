{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Shaderc.EnumAuto where

import Language.Haskell.TH

(: []) <$> do
	enms <- runIO $ lines <$> readFile "data/ShaderKind.txt"
	dataD (cxt []) (mkName "ShaderKind") [] Nothing
		((\enm -> normalC (mkName enm) []) <$> enms)
		[derivClause Nothing [conT ''Show]]
