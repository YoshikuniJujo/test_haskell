{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CEnum.SampleType where

import Language.Haskell.TH

do	is <- lines <$> runIO (readFile "th/enumSample.txt")
	(: []) <$> dataD (pure []) (mkName "EnumSample") [] Nothing
		((`normalC` []) . mkName <$> is)
		[derivClause Nothing [conT ''Show]]
