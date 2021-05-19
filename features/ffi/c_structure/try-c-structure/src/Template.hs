{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Template where

import Language.Haskell.TH
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal
import Control.Monad
import Control.Monad.Primitive
import Data.Bool
import Data.List
import Data.Array
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
	sigD (mkName $ lcfirst nt) $ conT (mkName nt) .-> tupT (conT <$> ts),
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

(.->) :: TypeQ -> TypeQ -> TypeQ
t1 .-> t2 = arrowT `appT` t1 `appT` t2

tupE' :: [ExpQ] -> ExpQ
tupE' [e] = e
tupE' es = tupE es

tupT :: [TypeQ] -> TypeQ
tupT [t] = t
tupT ts = foldl appT (tupleT $ length ts) ts

infixr 8 .$

(.$), (...), (.<$>), (.<*>), (.>>=), (.&&), (.||), (.==), (.<=), (.<) :: ExpQ -> ExpQ -> ExpQ
e1 .$ e2 = infixE (Just e1) (varE '($)) (Just e2)
e1 ... e2 = infixE (Just e1) (varE '(.)) (Just e2)
e1 .<$> e2 = infixE (Just e1) (varE '(<$>)) (Just e2)
e1 .<*> e2 = infixE (Just e1) (varE '(<*>)) (Just e2)
e1 .>>= e2 = infixE (Just e1) (varE '(>>=)) (Just e2)
e1 .&& e2 = infixE (Just e1) (varE '(&&)) (Just e2)
e1 .|| e2 = infixE (Just e1) (varE '(||)) (Just e2)
e1 .== e2 = infixE (Just e1) (varE '(==)) (Just e2)
e1 .<= e2 = infixE (Just e1) (varE '(<=)) (Just e2)
e1 .< e2 = infixE (Just e1) (varE '(<)) (Just e2)

(.+), (.*), zp :: ExpQ -> ExpQ -> ExpQ
e1 .+ e2 = infixE (Just e1) (varE '(+)) (Just e2)
e1 .* e2 = infixE (Just e1) (varE '(*)) (Just e2)
e1 `zp` e2 = infixE (Just e1) (varE 'zip) (Just e2)

pt :: ExpQ -> ExpQ -> ExpQ
e `pt` op = infixE (Just e) op Nothing

pp :: String -> ExpQ
pp s = litE (StringL s) `pt` varE '(++)

lcfirst, ucfirst :: String -> String
lcfirst = \case "" -> ""; c : cs -> toLower c : cs
ucfirst = \case "" -> ""; c : cs -> toUpper c : cs

mkPatternSig :: String -> [Name] -> DecQ
mkPatternSig nt ts =
	patSynSigD (mkName nt) $ foldr (.->) (conT $ mkName nt) (conT <$> ts)

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

mkInstanceEq :: String -> [String] -> DecQ
mkInstanceEq nt fs = do
	s1 <- newName "s1"
	s2 <- newName "s2"
	instanceD (cxt []) (conT ''Eq `appT` conT (mkName nt)) [
		funD '(==) [
			clause [varP s1, varP s2] (normalB
				$ foldl (.&&) (conE 'True) $ fieldEqual s1 s2 nt <$> fs
				) []
			]
		]

fieldEqual :: Name -> Name -> String -> String -> ExpQ
fieldEqual s1 s2 nt f_ = (f `appE` varE s1) .== (f `appE` varE s2)
	where
	f = varE . mkName $ lcfirst nt ++ ucfirst f_

mkInstanceOrd :: String -> [String] -> DecQ
mkInstanceOrd nt fs_ = do
	s1 <- newName "s1"
	s2 <- newName "s2"
	instanceD (cxt []) (conT ''Ord `appT` conT (mkName nt)) [
		funD '(<=) [
			clause [varP s1, varP s2] (normalB
				$ varE 'foldr `appE` lamOrd s1 s2 `appE`
					conE 'True `appE` listE fs
				) []
			]
		]
	where fs = varE . mkName . (lcfirst nt ++) . ucfirst <$> fs_

lamOrd :: Name -> Name -> ExpQ
lamOrd s1 s2 = do
	x <- newName "x"
	v <- newName "v"
	lamE [varP x, varP v] $ (varE x `appE` varE s1) .< (varE x `appE` varE s2) .||
		(((varE x `appE` varE s1) .== (varE x `appE` varE s2)) .&& varE v)

mkInstanceBounded :: String -> [String] -> DecQ
mkInstanceBounded nt fs =
	instanceD (cxt []) (conT ''Bounded `appT` conT (mkName nt)) [
		valD (varP 'minBound) (normalB $ foldl appE (conE $ mkName nt)
			(replicate (length fs) (varE 'minBound))) [],
		valD (varP 'maxBound) (normalB $ foldl appE (conE $ mkName nt)
			(replicate (length fs) (varE 'maxBound))) [] ]

mkIxRange :: Name -> String -> [String] -> DecQ
mkIxRange fn nt fs = do
	vs <- replicateM (length fs) $ newName "v"
	ws <- replicateM (length fs) $ newName "w"
	is <- replicateM (length fs) $ newName "i"
	funD fn [clause [tupP [conP (mkName nt) $ varP <$> vs, conP (mkName nt) $ varP <$> ws]] (normalB
			. compE $ (++ [noBindS . foldl appE (conE $ mkName nt) $ varE <$> is])
				$ (\(i, (v, w)) -> bindS (varP i) $ varE 'range `appE` tupE [varE v, varE w])
					<$> is `zip` (vs `zip` ws)
			) []
		]

mkIxIndex :: Name -> String -> [String] -> DecQ
mkIxIndex fn nt fs = do
	vs <- replicateM (length fs) $ newName "v"
	ws <- replicateM (length fs) $ newName "w"
	is <- replicateM (length fs) $ newName "i"
	funD fn [clause
		[	tupP [conP (mkName nt) $ varP <$> vs, conP (mkName nt) $ varP <$> ws],
			conP (mkName nt) $ varP <$> is ]
		(normalB
			$ varE 'foldl `appE` mkIxIndexLam `appE` litE (integerL 0)
				.$ (listE $ varE <$> vs) `zp` (listE $ varE <$> ws) `zp` (listE $ varE <$> is)
			)
		[]]

mkIxIndexLam :: ExpQ
mkIxIndexLam = do
	v <- newName "v"
	z <- newName "z"
	k <- newName "k"
	lamE [varP v, tupP [varP z, varP k]]
		$ (varE 'index `appE` varE z `appE` varE k) .+ (varE 'rangeSize `appE` varE z .* varE v)

mkIxInRange :: Name -> String -> [String] -> DecQ
mkIxInRange fn nt fs = do
	vs <- replicateM (length fs) $ newName "v"
	ws <- replicateM (length fs) $ newName "w"
	is <- replicateM (length fs) $ newName "i"
	funD fn [clause
		[	tupP [conP (mkName nt) $ varP <$> vs, conP (mkName nt) $ varP <$> ws],
			conP (mkName nt) $ varP <$> is ]
		(normalB . foldr (.&&) (conE 'True)
			$ (\((v, w), i) -> varE 'inRange `appE` tupE [varE v, varE w] `appE` varE i)
				<$> vs `zip` ws `zip` is
			)
		[]]

mkNewtypePrim :: String -> [Name] -> DecQ
mkNewtypePrim nt ds = do
	s <- newName "s"
	newtypeD (cxt []) (mkName $ nt ++ "Prim") [plainTV s] Nothing (normalC (mkName $ nt ++ "Prim") [
		bangType
			(bang noSourceUnpackedness noSourceStrictness)
			(conT ''ForeignPtr `appT` (conT (mkName $ nt ++ "Prim") `appT` varT s))
		]) [derivClause Nothing $ conT <$> ds]

mkFreezeSig :: String -> DecQ
mkFreezeSig nt = do
	m <- newName "m"
	sigD (mkName $ lcfirst nt ++ "Freeze") . forallT [] (cxt [conT ''PrimMonad `appT` varT m])
		$ conT (mkName $ nt ++ "Prim") `appT` (conT ''PrimState `appT` varT m) .-> (varT m `appT` conT (mkName nt))

mkFreezeFun :: String -> Name -> Name -> DecQ
mkFreezeFun nt frz fr = do
	ff <- newName "ff"
	funD (mkName $ lcfirst nt ++ "Freeze") [
		clause [conP (mkName $ nt ++ "Prim") [varP ff]] (
			normalB (mkFreezeBody nt frz fr ff)
			) []
		]

mkFreezeBody :: String -> Name -> Name -> Name -> ExpQ
mkFreezeBody nt frz fr ff = (varE 'unsafeIOToPrim ... (conE (mkName $ nt ++ "_") `pt` varE '(<$>)))
	.$ (varE 'withForeignPtr `appE` varE ff `appE` varE frz) .>>= (varE 'newForeignPtr .<$> varE 'id .<*> varE fr)

mkThawSig :: String -> DecQ
mkThawSig nt = do
	m <- newName "m"
	sigD (mkName $ lcfirst nt ++ "Thaw") . forallT [] (cxt [conT ''PrimMonad `appT` varT m])
		$ conT (mkName nt) .-> (varT m `appT` (conT (mkName $ nt ++ "Prim") `appT` (conT ''PrimState `appT` varT m)))

mkThawFun :: String -> Name -> Name -> DecQ
mkThawFun nt frz fr = do
	ff <- newName "ff"
	funD (mkName $ lcfirst nt ++ "Thaw") [
		clause [conP (mkName $ nt ++ "_") [varP ff]] (
			normalB (mkThawBody nt frz fr ff)
			) []
		]

mkThawBody :: String -> Name -> Name -> Name -> ExpQ
mkThawBody nt frz fr ff = (varE 'unsafeIOToPrim ... (conE (mkName $ nt ++ "Prim") `pt` varE '(<$>)))
	.$ (varE 'withForeignPtr `appE` varE ff `appE` varE frz) .>>= (varE 'newForeignPtr .<$> varE 'id .<*> varE fr)

mkCopySig :: String -> DecQ
mkCopySig nt = do
	m <- newName "m"
	sigD (mkName $ lcfirst nt ++ "Copy") . forallT [] (cxt [conT ''PrimMonad `appT` varT m])
		$ conT (mkName $ nt ++ "Prim") `appT` (conT ''PrimState `appT` varT m) .->
			(varT m `appT` (conT (mkName $ nt ++ "Prim") `appT` (conT ''PrimState `appT` varT m)))

mkCopyFun :: String -> Name -> Name -> DecQ
mkCopyFun nt frz fr = do
	ff <- newName "ff"
	funD (mkName $ lcfirst nt ++ "Copy") [
		clause [conP (mkName $ nt ++ "Prim") [varP ff]] (
			normalB (mkCopyBody nt frz fr ff)
			) []
		]

mkCopyBody :: String -> Name -> Name -> Name -> ExpQ
mkCopyBody nt frz fr ff = (varE 'unsafeIOToPrim ... (conE (mkName $ nt ++ "Prim") `pt` varE '(<$>)))
	.$ (varE 'withForeignPtr `appE` varE ff `appE` varE frz) .>>= (varE 'newForeignPtr .<$> varE 'id .<*> varE fr)
