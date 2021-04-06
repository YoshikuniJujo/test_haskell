{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module PatternSynonymsTh where

import Language.Haskell.TH

data Foo

fooSig, foo :: DecQ
fooSig = patSynSigD ''Foo (conT ''Foo)

foo = patSynD ''Foo (prefixPatSyn []) (explBidir [clause [] (normalB (litE (IntegerL 88))) []]) (litP (IntegerL 8))

mkPattern :: Name -> String -> Integer -> DecsQ
mkPattern t n v = sequence [
	patSynSigD (mkName n) (conT t),
	patSynD (mkName n) (prefixPatSyn []) (explBidir [clause [] (normalB (litE (IntegerL v))) []]) (litP (IntegerL v))
	]
