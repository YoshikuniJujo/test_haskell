{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Template.Tools (
	Q, Dec, Name, newName,

	conT, varT, appT, arrT, arrK, eqT, tupT,

	sigD, classD, openTypeFamilyD,

	noSig, plainTV, cxt,

	nameSwizzle, nameSwizzleXyz,
	nameGswizzle, nameGxU, nameGxL, nameXU,

	prodT, prodP, prodE

	) where

import GHC.Generics
import Language.Haskell.TH
import Data.Maybe
import Data.List qualified as L
import Data.Bool
import Data.Char

classGswizzle :: Int -> Q Dec
classGswizzle i =
	newName "a" >>= \a -> newName "s" >>= \s -> newName "t" >>= \t ->
	classD (cxt [])
		(nameGswizzle i) [plainTV s, plainTV t] [] [typeGx i t, sigGx i s t a]

typeGx :: Int -> Name -> Q Dec
typeGx i t = openTypeFamilyD (nameGxU i) [plainTV t] noSig Nothing

sigGx :: Int -> Name -> Name -> Name -> Q Dec
sigGx i s t a = sigD (nameGxL i) $
	(varT s `appT` varT a) `arrT`
	(conT (nameGxU i) `appT` varT t) `arrT` (varT t `appT` varT a)

instanceGswizzle1K1 :: Q Dec
instanceGswizzle1K1 =
	newName "a" >>= \a -> newName "i" >>= \i ->
	newName "b" >>= \b -> newName "i'" >>= \i' ->
	newName "v" >>= \v ->
	instanceD (cxt [])
		(conT (mkName "GSwizzleSet1") `appT` k1ia i a `appT` k1ia i' b)
		[typeGxK1 i' b, funGxK1 v]

k1ia :: Name -> Name -> TypeQ
k1ia i a = conT ''K1 `appT` varT i `appT` varT a

typeGxK1 :: Name -> Name -> Q Dec
typeGxK1 i a = tySynInstD
	$ tySynEqn Nothing (conT (mkName "GX") `appT` k1ia i a) (varT a)

funGxK1 :: Name -> Q Dec
funGxK1 v = funD (mkName "gx") [
	clause [conP 'K1 [wildP], varP v] (normalB $ conE 'K1 `appE` varE v) []
	]

instanceGswizzleM1 :: Int -> Q Dec
instanceGswizzleM1 n =
	newName "i" >>= \i -> newName "c" >>= \c -> newName "a" >>= \a ->
	newName "i'" >>= \i' -> newName "c'" >>= \c' -> newName "b" >>= \b ->
	newName "s" >>= \s -> newName "v" >>= \v ->
	instanceD (cxtGswizzleM1 n a b)
		(conT (nameGswizzle n) `appT` m1ica i c a `appT` m1ica i' c' b)
		[typeGxM1 n i' c' b, funGxM1 n s v]

cxtGswizzleM1 :: Int -> Name -> Name -> CxtQ
cxtGswizzleM1 i a b = cxt [conT (nameGswizzle i) `appT` varT a `appT` varT b]

m1ica :: Name -> Name -> Name -> TypeQ
m1ica i c a = conT ''M1 `appT`
	varT i `appT` varT c `appT` varT a

typeGxM1 :: Int -> Name -> Name -> Name -> Q Dec
typeGxM1 n i c a = tySynInstD $ tySynEqn Nothing
	(conT (nameGxU n) `appT` m1ica i c a)
	(conT (nameGxU n) `appT` varT a)

funGxM1 :: Int -> Name -> Name -> Q Dec
funGxM1 i s v = funD (nameGxL i) [clause
	[conP 'M1 [varP s], varP v]
	(normalB $ conE 'M1 `appE`
		(varE (nameGxL i) `appE` varE s `appE` varE v)) []]

instanceGswizzleProd :: Int -> Q Dec
instanceGswizzleProd n =
	newName "i" >>= \i -> newName "c" >>= \c -> newName "a" >>= \a ->
	newName "i'" >>= \i' -> newName "c'" >>= \c' -> newName "b" >>= \b ->
	newName "g" >>= \g -> newName "g'" >>= \g' ->
	newName "s" >>= \s -> newName "t" >>= \t -> newName "v" >>= \v ->
	instanceD (cxtGswizzleProd n a b g g')
		(instanceHeadProd n i c a g i' c' b g')
		[typeGxProd n i c a g i' c' b g', funGxProd n v s t]

instanceHeadProd :: Int ->
	Name -> Name -> Name -> Name -> Name -> Name -> Name -> Name -> TypeQ
instanceHeadProd n i c a g i' c' b g' = case n of
	1 -> conT (nameGswizzle n) `appT` aProdG i c a g `appT` aProdG i' c' b g
	_ -> conT (nameGswizzle n) `appT` aProdG i c a g `appT` aProdG i c a g'

typeGxProd :: Int ->
	Name -> Name -> Name -> Name -> Name -> Name -> Name -> Name -> Q Dec
typeGxProd n i c a g i' c' b g' = tySynInstD $ tySynEqn Nothing
	(case n of
		1 -> conT (nameGxU n) `appT` aProdG i' c' b g
		_ -> conT (nameGxU n) `appT` aProdG i c a g')
	(conT (nameGxxyU n) `appT` varT (nameAOrG n b g'))

aProdG :: Name -> Name -> Name -> Name -> TypeQ
aProdG i c a g =
	(conT ''M1 `appT` varT i `appT` varT c `appT` varT a) `prodT`
	varT g

funGxProd :: Int -> Name -> Name -> Name -> Q Dec
funGxProd n v s t = funD (nameGxL n) [
	clause [infixP (varP s) '(:*:) (varP t), varP v]
		(normalB
		. prodPostOrPre n (varE $ nameGOrA n s t)
		$ varE (nameGxxyL n)
			`appE` varE (nameAOrG n s t)
			`appE` varE v
			) [] ]

prodPostOrPre :: Int -> ExpQ -> ExpQ -> ExpQ
prodPostOrPre 1 e b = infixE (Just b) (conE '(:*:)) (Just e)
prodPostOrPre _ e b = infixE (Just e) (conE '(:*:)) (Just b)

cxtGswizzleProd :: Int -> Name -> Name -> Name -> Name -> CxtQ
cxtGswizzleProd n a b g g' = case n of
	1 -> cxt [conT (nameGswizzle 1) `appT` varT a `appT` varT b]
	_ -> cxt [conT (nameGswizzle $ n - 1) `appT` varT g `appT` varT g']

instanceGswizzleProdProd :: Int -> Q Dec
instanceGswizzleProdProd n =
	newName "a" >>= \a -> newName "b" >>= \b -> newName "c" >>= \c ->
	newName "a'" >>= \a' -> newName "b'" >>= \b' -> newName "c'" >>= \c' ->
	newName "x" >>= \x -> newName "y" >>= \y -> newName "z" >>= \z ->
	newName "v" >>= \v ->
	instanceD (cxtGswizzleProdProd n a b c a' b' c')
		(conT (nameGswizzle n)
			`appT` aProdBProdCT' a b c
			`appT` aProdBProdCT' a' b' c')
		[typeGxProdProd n a' b' c', funGxProdProd n v x y z]

cxtGswizzleProdProd :: Int ->
	Name -> Name -> Name -> Name -> Name -> Name -> CxtQ
cxtGswizzleProdProd n a b c a' b' c' = cxt [
	conT (nameGswizzle n)
		`appT` aProdBProdCT a b c `appT` aProdBProdCT a' b' c' ]

typeGxProdProd :: Int -> Name -> Name -> Name -> Q Dec
typeGxProdProd i a b c = tySynInstD $ tySynEqn Nothing
	(conT (nameGxU i) `appT` aProdBProdCT' a b c)
	(conT (nameGxU i) `appT` aProdBProdCT a b c)

funGxProdProd :: Int -> Name -> Name -> Name -> Name -> Q Dec
funGxProdProd i v a b c = funD (nameGxL i) [clause
	[	
		aProdBProdCP' a b c,
		varP v
		]
	(normalB $ letProdProd i v a b c) []]

letProdProd :: Int -> Name -> Name -> Name -> Name -> ExpQ
letProdProd i v a b c = newName "x" >>= \x -> newName "y" >>= \y -> newName "z" >>= \z ->
	letE [valD (aProdBProdCP x y z) (normalB $
			varE (nameGxL i)
				`appE` aProdBProdCE a b c
				`appE` varE v
			) []]
		(aProdBProdCE2 x y z)

aProdBProdCT, aProdBProdCT' :: Name -> Name -> Name -> TypeQ
aProdBProdCT a b c = varT a `prodT` varT b `prodT` varT c
aProdBProdCT' a b c = (varT a `prodT` varT b) `prodT` varT c

aProdBProdCE2 :: Name -> Name -> Name -> ExpQ
aProdBProdCE2 a b c = (varE a `prodE` varE b) `prodE` varE c

aProdBProdCE :: Name -> Name -> Name -> ExpQ
aProdBProdCE a b c = varE a `prodE` varE b `prodE` varE c

aProdBProdCP :: Name -> Name -> Name -> PatQ
aProdBProdCP a b c = varP a `prodP` (varP b `prodP` varP c)

aProdBProdCP' :: Name -> Name -> Name -> PatQ
aProdBProdCP' a b c = (varP a `prodP` varP b) `prodP` varP c

classSwizzleClass :: Int -> Q Dec
classSwizzleClass n =
	newName "a" >>= \a -> newName "b" >>= \b ->
	newName "s" >>= \s -> newName "v" >>= \v ->
	classD (classSwizzleContext n a b) (nameSwizzle n) [plainTV a, plainTV b] [] [
		typeX n b,
		sigX n a b,
		defaultX n a b,
		defaultFunX n s v ]

nameSwizzleXyz :: Char -> Name
nameSwizzleXyz = nameSwizzle
	. (+ 1) . fromJust . (`L.elemIndex` ("xyz" ++ reverse ['a' .. 'w']))

nameSwizzle :: Int -> Name
nameSwizzle = mkName . ("SwizzleSet" ++) . show

typeX :: Int -> Name -> Q Dec
typeX i a = openTypeFamilyD (nameXU i) [plainTV a] noSig Nothing

nameXU :: Int -> Name
nameXU = mkName . (: "") . toUpper . alphabet

sigX :: Int -> Name -> Name -> Q Dec
sigX i a b = sigD (nameXL i) $
	varT a `arrT` (conT (nameXU i) `appT` varT b) `arrT` varT b

nameXL :: Int -> Name
nameXL = mkName . (: "") . alphabet

defaultX :: Int -> Name -> Name -> Q Dec
defaultX i a b = defaultSigD (nameXL i) . forallT [] (defaultXContext i a b) $
	varT a `arrT` ((conT (nameXU i) `appT` varT b) `arrT` varT b)

defaultXContext :: Int -> Name -> Name -> CxtQ
defaultXContext i a b = cxt [
	conT ''Generic `appT` varT a, conT ''Generic `appT` varT b,
	conT (nameGswizzle i)
		`appT` (conT ''Rep `appT` varT a)
		`appT` (conT ''Rep `appT` varT b),
	(conT (nameXU i) `appT` varT b) `eqT`
		(conT (nameGxU i) `appT` (conT ''Rep `appT` varT b))
	]

defaultFunX :: Int -> Name -> Name -> Q Dec
defaultFunX i s v = funD (nameXL i) [clause [varP s, varP v]
--	(normalB $ varE 'to `comE` (varE (nameGxL i) `appE` varE v) `comE` varE 'from) []]
	(normalB $
		varE 'to `appE` (varE (nameGxL i)
			`appE` (varE 'from `appE` varE s)
			`appE` varE v
			))
	[]]
--	[]
--	$ infixE (Just . varE $ nameGxL i) (varE '(.)) (Just $ varE 'from)) []

eqT :: TypeQ -> TypeQ -> TypeQ
t1 `eqT` t2 = equalityT `appT` t1 `appT` t2

classSwizzleContext :: Int -> Name -> Name -> CxtQ
classSwizzleContext n a b = case n of
	_ -> cxt []
--	1 -> cxt []
	_ -> cxt [conT (nameSwizzle $ n - 1) `appT` varT a `appT` varT b]

instanceSwizzleTuple :: Int -> DecsQ
instanceSwizzleTuple n = (++)
	<$> (`instanceSwizzleTuple_` n) `mapM` [1 .. min n 26]
	<*> deriveGeneric n

instanceSwizzleTuple_ :: Int -> Int -> Q Dec
instanceSwizzleTuple_ i n =
	mapM (newName . (vars !!)) [0 .. n - 1] >>= \ns -> newName "x" >>= \a ->
	let	ns' = setX ns (i - 1) a in
	instanceD (cxt [])
		(conT (nameSwizzle i) `appT` tupT ns `appT` tupT ns')
		[typeXFromTuple i ns']

setX :: [a] -> Int -> a -> [a]
setX [] _ _ = []
setX (_ : xs) 0 y = y : xs
setX (x : xs) i y = x : setX xs (i - 1) y

deriveGeneric :: Int -> DecsQ
deriveGeneric i = do
	t <- tupT =<< newNameAbc i
	isInstance ''Generic [t] >>= bool
		((: []) <$> standaloneDerivD (cxt [])
			(conT ''Generic `appT` pure t))
		(pure [])

newNameAbc :: Int -> Q [Name]
newNameAbc i = newName `mapM` take i vars

vars :: [String]
vars = ((: "") <$> ['a' .. 'z']) ++ [ cs ++ [c] | cs <- vars, c <- ['a' .. 'z'] ]

tupT :: [Name] -> TypeQ
tupT ns = foldl appT (tupleT $ length ns) $ varT <$> ns

typeXFromTuple :: Int -> [Name] -> Q Dec
typeXFromTuple i ns = tySynInstD $ tySynEqn Nothing (conT (nameXU i) `appT` tupT ns) (varT $ ns !! (i - 1))

nameGswizzle :: Int -> Name
nameGswizzle = mkName . ("GSwizzleSet" ++) . show

nameGxU :: Int -> Name
nameGxU i = mkName $ "G" ++ [toUpper $ alphabet i]

nameGxL :: Int -> Name
nameGxL i = mkName $ "g" ++ [alphabet i]

nameGxxyU :: Int -> Name
nameGxxyU = \case 1 -> nameGxU 1; i -> nameGxU $ i - 1

nameAOrG :: Int -> Name -> Name -> Name
nameAOrG i a b = case i of 1 -> a; _ -> b

nameGOrA :: Int -> Name -> Name -> Name
nameGOrA i a b = case i of 1 -> b; _ -> a

aOrWildP :: Int -> Name -> PatQ
aOrWildP i a = case i of 1 -> varP a; _ -> wildP

bOrWildP :: Int -> Name -> PatQ
bOrWildP i b = case i of 1 -> wildP; _ -> varP b

infixr 9 `prodT`, `prodE`, `prodP`

prodT :: TypeQ -> TypeQ -> TypeQ
t1 `prodT` t2 = conT ''(:*:) `appT` t1 `appT` t2

prodE :: ExpQ -> ExpQ -> ExpQ
e1 `prodE` e2 = conE '(:*:) `appE` e1 `appE` e2

prodP :: PatQ -> PatQ -> PatQ
p1 `prodP` p2 = infixP p1 '(:*:) p2

nameGxxyL :: Int -> Name
nameGxxyL = \case 1 -> nameGxL 1; i -> nameGxL $ i - 1

alphabet :: Int -> Char
alphabet i | i > 26 = error $ "no such alphabet: " ++ show i
alphabet i = (("xyz" ++ reverse ['a' .. 'w']) !!) $ subtract 1 i

infixr 7 `arrT`

arrT :: TypeQ -> TypeQ -> TypeQ
t1 `arrT` t2 = arrowT `appT` t1 `appT` t2

infixr 6 `arrK`

arrK :: Kind -> Kind -> Kind
k1 `arrK` k2 = arrowK `appK` k1 `appK` k2
