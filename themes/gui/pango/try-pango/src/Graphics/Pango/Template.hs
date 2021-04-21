{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Template where

import Language.Haskell.TH

mkMemberGen :: Name -> Name -> String -> Integer -> DecsQ
mkMemberGen t c n v = sequence [
	patSynSigD (mkName n) (conT t),
	patSynD (mkName n) (prefixPatSyn [])
		(explBidir [clause [] (normalB (conE c `appE` litE (IntegerL v))) []])
		(conP c [litP (IntegerL v)])
	]
