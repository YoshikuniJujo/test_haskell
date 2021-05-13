{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Template where

import Language.Haskell.TH
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal
import Control.Monad
import Data.Bool
import Data.List
import Data.Char
import System.IO.Unsafe
import Text.Read

mkNewtype :: String -> DecQ
mkNewtype nt = newtypeD (cxt []) (mkName nt) [] Nothing (normalC (mkName $ nt ++ "_") [
	bangType
		(bang noSourceUnpackedness noSourceStrictness)
		(conT ''ForeignPtr `appT` conT (mkName nt))
	]) []

mkPatternFun :: String -> [(Name, ExpQ)] -> DecsQ
mkPatternFun nt tpks = sequence [
	sigD (mkName $ lcfirst nt) $ conT (mkName nt) `arrT` tupT (conT <$> ts),
	funD (mkName $ lcfirst nt) [do
		ff <- newName "ff"
		pf <- newName . bool "pf" "_" $ null tpks
		clause [conP (mkName $ nt ++ "_") [varP ff]] (
			normalB $ varE 'unsafePerformIO .$
				varE 'withForeignPtr `appE` varE ff
					`appE` lamE [varP pf] (mkPatternFunDo pf pks)
			) []
		] ]
	where (ts, pks) = unzip tpks

mkPatternFunDo :: Name -> [ExpQ] -> ExpQ
mkPatternFunDo pf pks = do
	fs <- replicateM (length pks) $ newName "f"
	doE . (++ [noBindS $ varE 'pure `appE` tupE' (varE <$> fs)]) $ (<$> (fs `zip` pks)) \(f', pk') ->
		bindS (varP f') $ pk' `appE` varE pf

arrT :: TypeQ -> TypeQ -> TypeQ
arrT t1 t2 = arrowT `appT` t1 `appT` t2

tupE' :: [ExpQ] -> ExpQ
tupE' [e] = e
tupE' es = tupE es

tupT :: [TypeQ] -> TypeQ
tupT [t] = t
tupT ts = foldl appT (tupleT $ length ts) ts

infixr 8 .$

(.$), (...), (.<$>) :: ExpQ -> ExpQ -> ExpQ
e1 .$ e2 = infixE (Just e1) (varE '($)) (Just e2)
e1 ... e2 = infixE (Just e1) (varE '(.)) (Just e2)
e1 .<$> e2 = infixE (Just e1) (varE '(<$>)) (Just e2)

pt :: ExpQ -> ExpQ -> ExpQ
e `pt` op = infixE (Just e) op Nothing

pp :: String -> ExpQ
pp s = litE (StringL s) `pt` varE '(++)

lcfirst, ucfirst :: String -> String
lcfirst = \case "" -> ""; c : cs -> toLower c : cs
ucfirst = \case "" -> ""; c : cs -> toUpper c : cs

mkPatternSig :: String -> [Name] -> DecQ
mkPatternSig nt ts =
	patSynSigD (mkName nt) $ foldr arrT (conT $ mkName nt) (conT <$> ts)

mkPatternBody :: String -> Integer -> [String] -> [ExpQ] -> DecQ
mkPatternBody nt sz fs poks =
	patSynD (mkName nt) (recordPatSyn fs')
		(explBidir [mkPatternBodyClause nt sz poks])
		(viewP (varE . mkName $ lcfirst nt) (tupP $ varP <$> fs'))
	where
	fs' = mkName . (lcfirst nt ++) . ucfirst <$> fs

mkPatternBodyClause :: String -> Integer -> [ExpQ] -> ClauseQ
mkPatternBodyClause nt sz pks = do
	vs <- replicateM (length pks) $ newName "v"
	p <- newName "p"
	clause (varP <$> vs) (
		normalB $ varE 'unsafePerformIO .$ conE (mkName nt_) .<$> doE (
			bindS (varP p) (varE 'mallocBytes `appE` litE (IntegerL sz)) :
			((<$> zip pks vs) \(pk, v) -> noBindS $ pk `appE` varE p `appE` varE v) ++
			[noBindS $ varE 'newForeignPtr `appE` varE p `appE` (varE 'free `appE` varE p)]
			)
		) []
	where
	nt_ = nt ++ "_"

mkInstanceShow :: String -> [String] -> DecQ
mkInstanceShow nt fs = do
	f <- newName "f"
	vs <- replicateM (length fs) $ newName "v"
	instanceD (cxt []) (conT ''Show `appT` conT (mkName nt)) [
		funD 'showsPrec [
			clause [wildP, varP f] (
				normalB $ pp (nt ++ " {") ...
					mkShowFields nt fs vs ...
					pp "}"
				) [valD (conP (mkName nt) $ varP <$> vs) (normalB $ varE f) []] ] ]

litI :: Integer -> ExpQ
litI = litE . IntegerL

mkShowFields :: String -> [String] -> [Name] -> ExpQ
mkShowFields nt fs ns = foldr (...) (varE 'id) . intersperse (pp ", ")
	$ (<$> zip fs ns) \(f, n) -> pp (lcfirst nt ++ ucfirst f ++ " = ") ... (varE 'showsPrec `appE` litI 11 `appE` varE n)

mkInstanceRead :: String -> [String] -> DecQ
mkInstanceRead nt fs = do
	vs <- replicateM (length fs) $ newName "v"
	instanceD (cxt []) (conT ''Read `appT` conT (mkName nt)) [
		valD (varP 'readPrec) (normalB $ varE 'parens .$ varE 'prec `appE` litI 10 `appE` doE ([
			bindS (conP 'Ident [litP $ StringL "Foo"]) $ varE 'lexP,
			bindS (conP 'Punc [litP $ StringL "{"]) $ varE 'lexP] ++
			mkReadFields nt fs vs ++
			[bindS (conP 'Punc [litP $ StringL "}"]) $ varE 'lexP,
			noBindS $ varE 'pure .$ foldl appE (conE $ mkName nt) (varE <$> vs)
			])) []
		]

mkReadFields :: String -> [String] -> [Name] -> [StmtQ]
mkReadFields nt fs vs = intercalate [bindS (conP 'Punc $ [litP $ StringL ","]) $ varE 'lexP] $ (<$> zip fs vs) \(f, v) -> [
	bindS (conP 'Ident [litP . StringL $ lcfirst nt ++ ucfirst f]) $ varE 'lexP,
	bindS (conP 'Punc [litP $ StringL "="]) $ varE 'lexP,
	bindS (varP v) $ varE 'step `appE` varE 'readPrec ]
