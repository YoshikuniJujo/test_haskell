{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Template where

import Language.Haskell.TH
import Text.Read

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
		clause [varP d] (normalB (lamCaseE
			((matchFoo <$> ns) ++
			[match (conP (mkName t) [varP n]) (normalB $ foo d n) []])
			)) []
		]
	where
	foo d n = varE 'showParen `appE` (varE d `gt` litE (IntegerL 10))
		`dl` ((litE (StringL (t ++ " ")) `p` varE '(++))
			`dt` (varE 'showsPrec `appE` litE (IntegerL 11) `appE` varE n))

matchFoo :: String -> MatchQ
matchFoo f = match (conP (mkName f) []) (normalB $ litE (StringL f) `p` (varE '(++))) []

gt :: Q Exp -> Q Exp -> Q Exp
e1 `gt` e2 = infixE (Just e1) (varE '(>)) (Just e2)

dl, fdl :: Q Exp -> Q Exp -> Q Exp
e1 `dl` e2 = infixE (Just e1) (varE '($)) (Just e2)
e1 `fdl` e2 = infixE (Just e1) (varE '(<$>)) (Just e2)

dt :: Q Exp -> Q Exp -> Q Exp
e1 `dt` e2 = infixE (Just e1) (varE '(.)) (Just e2)

p :: Q Exp -> Q Exp -> Q Exp
ex `p` op = infixE (Just ex) op Nothing

mkRead :: String -> [String] -> DecQ
mkRead t ns = instanceD (cxt []) (conT ''Read `appT` conT (mkName t)) . (: [])
	$ valD (varP 'readPrec) (normalB $ varE 'parens `dl` (varE 'choice `appE` listE (
		(readFoo <$> ns) ++
		[varE 'prec `appE` litE (IntegerL 10) `appE` doE [
			bindS (conP 'Ident [litP $ StringL t]) $ varE 'lexP,
			noBindS $ conE (mkName t) `fdl` (varE 'step `appE` varE 'readPrec) ]]
		))) []

readFoo :: String -> ExpQ
readFoo n = doE [
	bindS (conP 'Ident [litP $ StringL n]) $ varE 'lexP,
	noBindS $ varE 'pure `appE` conE (mkName n)
	]
