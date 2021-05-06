{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Template where

import Language.Haskell.TH

mkNewtype :: String -> Name -> DecQ
mkNewtype nt t = newtypeD (cxt []) (mkName nt) [] Nothing
	(normalC (mkName nt) [bangType (bang noSourceUnpackedness noSourceStrictness) (conT t)])
	[]

mkMembers :: String -> [(String, Integer)] -> DecsQ
mkMembers t nvs = concat <$> uncurry (mkMemberGen (mkName t) (mkName t)) `mapM` nvs

mkMemberGen :: Name -> Name -> String -> Integer -> DecsQ
mkMemberGen t c n v = sequence [
	patSynSigD (mkName n) (conT t),
	patSynD (mkName n) (prefixPatSyn [])
		(explBidir [clause [] (normalB (conE c `appE` litE (IntegerL v))) []])
		(conP c [litP (IntegerL v)])
	]

mkShow :: String -> [String] -> DecQ
mkShow t ns = instanceD (cxt []) (conT ''Show `appT` conT (mkName t))
	[defineShowsPrec t ns]

defineShowsPrec :: String -> [String] -> DecQ
defineShowsPrec t ns = do
	d <- newName "d"
	n <- newName "n"
	funD 'showsPrec [
		clause [varP d] (normalB (lamCaseE [
			match (conP (mkName t) [varP n]) (normalB
				$ foo d n) []
			])) []
		]
	where
	foo d n = varE 'showParen `appE` (varE d `gt` litE (IntegerL 10))
		`dl` ((litE (StringL (t ++ " ")) `p` varE '(++))
			`dt` (varE 'showsPrec `appE` litE (IntegerL 11) `appE` varE n))

gt :: Q Exp -> Q Exp -> Q Exp
e1 `gt` e2 = infixE (Just e1) (varE '(>)) (Just e2)

dl :: Q Exp -> Q Exp -> Q Exp
e1 `dl` e2 = infixE (Just e1) (varE '($)) (Just e2)

dt :: Q Exp -> Q Exp -> Q Exp
e1 `dt` e2 = infixE (Just e1) (varE '(.)) (Just e2)

p :: Q Exp -> Q Exp -> Q Exp
ex `p` op = infixE (Just ex) op Nothing
