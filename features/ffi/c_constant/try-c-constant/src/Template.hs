{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Template where

import Language.Haskell.TH

mkNewtype :: String -> Name -> DecQ
mkNewtype nt t = newtypeD (cxt []) (mkName nt) [] Nothing
	(normalC (mkName nt) [bangType (bang noSourceUnpackedness noSourceStrictness) (conT t)])
	[derivClause Nothing [conT ''Show]]

mkMemberGen :: Name -> Name -> String -> Integer -> DecsQ
mkMemberGen t c n v = sequence [
	patSynSigD (mkName n) (conT t),
	patSynD (mkName n) (prefixPatSyn [])
		(explBidir [clause [] (normalB (conE c `appE` litE (IntegerL v))) []])
		(conP c [litP (IntegerL v)])
	]
