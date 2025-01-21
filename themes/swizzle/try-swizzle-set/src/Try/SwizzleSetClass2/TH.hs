{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Try.SwizzleSetClass2.TH (
	classGswizzle, instanceGswizzle1K1,
	instanceGswizzleM1, instanceGswizzleProd, instanceGswizzleProdProd, tff,
	classSwizzleClass
	) where

import GHC.Generics
import Language.Haskell.TH hiding (Type)
import Data.Kind
import Data.Char

import Template.Tools

class GSwizzleSet1 s b where
	type GX s b :: k -> Type
	gx :: s a -> b -> GX s b a

classGswizzle :: Int -> Q Dec
classGswizzle n =
	newName "s" >>= \s -> newName "b" >>= \b -> newName "a" >>= \a ->
	classD (cxt []) (nameGswizzle n) [plainTV s, plainTV b] []
		[typeGx n s b, sigGx n s b a]

typeGx :: Int -> Name -> Name -> Q Dec
typeGx i s b = newName "k" >>= \k ->
	openTypeFamilyD (nameGxU i)
		[plainTV s, plainTV b] (kindSig $ varK k `arrK` conK ''Type) Nothing

sigGx :: Int -> Name -> Name -> Name -> Q Dec
sigGx i s b a = sigD (nameGxL i) $
	(varT s `appT` varT a)
	`arrT` varT b
	`arrT` (conT (nameGxU i) `appT` varT s `appT` varT b `appT` varT a)

instanceGswizzle1K1 :: Q Dec
instanceGswizzle1K1 =
	newName "i" >>= \i -> newName "a" >>= \a -> newName "b" >>= \b ->
	newName "v" >>= \v ->
	instanceD (cxt [])
		(conT (mkName "GSwizzleSet1") `appT` k1ia i a `appT` varT b)
		[typeGxK1 i a b, funGxK1 v]

k1ia :: Name -> Name -> TypeQ
k1ia i a = conT ''K1 `appT` varT i `appT` varT a

typeGxK1 :: Name -> Name -> Name -> Q Dec
typeGxK1 i a b = tySynInstD
	$ tySynEqn Nothing (conT (mkName "GX") `appT` k1ia i a `appT` varT b) (k1ia i b)

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
		(conT (nameGswizzle n) `appT` m1ica i c a `appT` varT b)
		[typeGxM1 n i c a b, funGxM1 n s v]

cxtGswizzleM1 :: Int -> Name -> Name -> CxtQ
cxtGswizzleM1 i a b = cxt [conT (nameGswizzle i) `appT` varT a `appT` varT b]

m1ica :: Name -> Name -> Name -> TypeQ
m1ica i c a = conT ''M1 `appT`
	varT i `appT` varT c `appT` varT a

m1ict :: Name -> Name -> TypeQ -> TypeQ
m1ict i c t = conT ''M1 `appT` varT i `appT` varT c `appT` t

typeGxM1 :: Int -> Name -> Name -> Name -> Name -> Q Dec
typeGxM1 n i c a b = tySynInstD $ tySynEqn Nothing
	(conT (nameGxU n) `appT` m1ica i c a `appT` varT b)
	(m1ict i c (conT (nameGxU n) `appT` varT a `appT` varT b))

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
	instanceD (cxtGswizzleProd n a g b)
		(instanceHeadProd n i c a g b)
		[typeGxProd n i c a g i' c' b g', funGxProd n v s t]

cxtGswizzleProd :: Int -> Name -> Name -> Name -> CxtQ
cxtGswizzleProd n a g b = case n of
	1 -> cxt [conT (nameGswizzle 1) `appT` varT a `appT` varT b]
	_ -> cxt [conT (nameGswizzle $ n - 1) `appT` varT g `appT` varT b]

instanceHeadProd :: Int ->
	Name -> Name -> Name -> Name -> Name -> TypeQ
instanceHeadProd n i c a g b = case n of
	1 -> conT (nameGswizzle n) `appT` aProdG i c a g `appT` varT b
	_ -> conT (nameGswizzle n) `appT` aProdG i c a g `appT` varT b

typeGxProd :: Int ->
	Name -> Name -> Name -> Name -> Name -> Name -> Name -> Name -> Q Dec
typeGxProd n i c a g i' c' b g' = tySynInstD $ tySynEqn Nothing
	(conT (nameGxU n) `appT` aProdG i c a g `appT` varT b)
	(case n of
		1 -> aProdGT
			i c (conT gxxyu `appT` varT a `appT` varT b) (varT g)
		_ -> aProdGT
			i c (varT a) (conT gxxyu `appT` varT g `appT` varT b))
	where gxxyu = nameGxxyU n

aProdG :: Name -> Name -> Name -> Name -> TypeQ
aProdG i c a g =
	(conT ''M1 `appT` varT i `appT` varT c `appT` varT a) `prodT`
	varT g

aProdGT :: Name -> Name -> TypeQ -> TypeQ -> TypeQ
aProdGT i c a g =
	(conT ''M1 `appT` varT i `appT` varT c `appT` a) `prodT` g

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

nameGxxyU :: Int -> Name
nameGxxyU = \case 1 -> nameGxU 1; i -> nameGxU $ i - 1

nameAOrG :: Int -> Name -> Name -> Name
nameAOrG i a b = case i of 1 -> a; _ -> b

nameGOrA :: Int -> Name -> Name -> Name
nameGOrA i a b = case i of 1 -> b; _ -> a

nameGxxyL :: Int -> Name
nameGxxyL = \case 1 -> nameGxL 1; i -> nameGxL $ i - 1

tff :: DecQ
tff = newName "s" >>= \s ->
	newName "a" >>= \a -> newName "b" >>= \b -> newName "c" >>= \c ->
	closedTypeFamilyD (mkName "F") [plainTV s] noSig Nothing [
		tySynEqn Nothing
			(conT (mkName "F") `appT` (varT a `prodT` (varT b `prodT` varT c)))
			((varT a `prodT` varT b) `prodT` varT c) ]

instanceGswizzleProdProd :: Int -> Q Dec
instanceGswizzleProdProd n =
	newName "a" >>= \a -> newName "b" >>= \b -> newName "c" >>= \c ->
	newName "v" >>= \vt ->
	newName "a'" >>= \a' -> newName "b'" >>= \b' -> newName "c'" >>= \c' ->
	newName "x" >>= \x -> newName "y" >>= \y -> newName "z" >>= \z ->
	newName "v" >>= \vf ->
	instanceD (cxtGswizzleProdProd n a b c vt b' c')
		(conT (nameGswizzle n)
			`appT` aProdBProdCT' a b c `appT` varT vt)
		[typeGxProdProd n a b c vt, funGxProdProd n vf x y z]

cxtGswizzleProdProd :: Int ->
	Name -> Name -> Name -> Name -> Name -> Name -> CxtQ
cxtGswizzleProdProd n a b c v b' c' = cxt [
	conT (nameGswizzle n) `appT` aProdBProdCT a b c `appT` varT v,
	conT (nameGxU n) `appT` aProdBProdCT a b c `appT` varT v `eqT`
	taProdBProdCT (conT (nameGxU n) `appT` varT a `appT` varT v) b c
	]

typeGxProdProd :: Int -> Name -> Name -> Name -> Name -> Q Dec
typeGxProdProd i a b c v = tySynInstD $ tySynEqn Nothing
	(conT (nameGxU i) `appT` aProdBProdCT' a b c `appT` varT v)
	(conT (mkName "F") `appT` (conT (nameGxU i) `appT` aProdBProdCT a b c `appT` varT v))

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

taProdBProdCT :: TypeQ -> Name -> Name -> TypeQ
taProdBProdCT a b c = a `prodT` varT b `prodT` varT c

aProdBProdCE2 :: Name -> Name -> Name -> ExpQ
aProdBProdCE2 a b c = (varE a `prodE` varE b) `prodE` varE c

aProdBProdCE :: Name -> Name -> Name -> ExpQ
aProdBProdCE a b c = varE a `prodE` varE b `prodE` varE c

aProdBProdCP :: Name -> Name -> Name -> PatQ
aProdBProdCP a b c = varP a `prodP` (varP b `prodP` varP c)

aProdBProdCP' :: Name -> Name -> Name -> PatQ
aProdBProdCP' a b c = (varP a `prodP` varP b) `prodP` varP c

infixr 9 `prodT`, `prodE`, `prodP`

prodT :: TypeQ -> TypeQ -> TypeQ
t1 `prodT` t2 = conT ''(:*:) `appT` t1 `appT` t2

prodE :: ExpQ -> ExpQ -> ExpQ
e1 `prodE` e2 = conE '(:*:) `appE` e1 `appE` e2

prodP :: PatQ -> PatQ -> PatQ
p1 `prodP` p2 = infixP p1 '(:*:) p2

classSwizzleClass :: Int -> Q Dec
classSwizzleClass n =
	newName "s" >>= \ta -> newName "b" >>= \b ->
	newName "s" >>= \fs -> newName "v" >>= \v ->
	classD (classSwizzleContext n ta b) (nameSwizzle n) [plainTV ta, plainTV b] [] [
		typeX n ta b,
		sigX n ta b,
		defaultX n ta b,
		defaultFunX n fs v ]

classSwizzleContext :: Int -> Name -> Name -> CxtQ
classSwizzleContext n a b = case n of
	_ -> cxt []
--	1 -> cxt []
	_ -> cxt [conT (nameSwizzle $ n - 1) `appT` varT a `appT` varT b]

nameSwizzle :: Int -> Name
nameSwizzle = mkName . ("SwizzleSet" ++) . show

typeX :: Int -> Name -> Name -> Q Dec
typeX i s b = openTypeFamilyD (nameXU i) [plainTV s, plainTV b] noSig Nothing

nameXU :: Int -> Name
nameXU = mkName . (: "") . toUpper . alphabet

sigX :: Int -> Name -> Name -> Q Dec
sigX i a b = sigD (nameXL i) $
	varT a `arrT` varT b `arrT` (conT (nameXU i) `appT` varT a `appT` varT b)

nameXL :: Int -> Name
nameXL = mkName . (: "") . alphabet

defaultX :: Int -> Name -> Name -> Q Dec
defaultX i a b = defaultSigD (nameXL i) . forallT [] (defaultXContext i a b) $
	varT a `arrT` varT b `arrT` (conT (nameXU i) `appT` varT a `appT` varT b)

defaultXContext :: Int -> Name -> Name -> CxtQ
defaultXContext i a b = cxt [
	conT ''Generic `appT` varT a,
	conT ''Generic `appT` (conT (nameXU i) `appT` varT a `appT` varT b),
	conT (nameGswizzle i)
		`appT` (conT ''Rep `appT` varT a)
		`appT` varT b,
	conT ''Rep `appT` (conT (nameXU i) `appT` varT a `appT` varT b) `eqT`
		(conT (nameGxU i) `appT` (conT ''Rep `appT` varT a) `appT` varT b)
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

alphabet :: Int -> Char
alphabet i | i > 26 = error $ "no such alphabet: " ++ show i
alphabet i = (("xyz" ++ reverse ['a' .. 'w']) !!) $ subtract 1 i

instance GSwizzleSet1 (K1 i a) b where
	type GX (K1 i a) b = K1 i b
	gx (K1 _) v = K1 v

instance GSwizzleSet1 a b => GSwizzleSet1 (M1 i c a) b where
	type GX (M1 i c a) b = M1 i c (GX a b)
	gx (M1 s) v = M1 $ gx s v

instance GSwizzleSet1 a b => GSwizzleSet1 (M1 i c a :*: g)  b where
	type GX (M1 i c a :*: g) b = M1 i c (GX a b) :*: g
	gx (s :*: t) v = gx s v :*: t

type family F s where
	F (a :*: (b :*: c)) = (a :*: b) :*: c

instance (
	GSwizzleSet1 (a :*: (b :*: c)) v,
	GX (a :*: (b :*: c)) v ~ GX a v :*: (b :*: c)
	) =>
	GSwizzleSet1 ((a :*: b) :*: c) v where
	type GX ((a :*: b) :*: c) v = F (GX (a :*: (b :*: c)) v)
	gx ((x :*: y) :*: z) v = let
		(x' :*: (y' :*: z')) = gx (x :*: (y :*: z)) v in
		((x' :*: y') :*: z')

class SwizzleSet1 s b where
	type X s b
	x :: s -> b -> X s b

	default x :: (
		Generic s, Generic (X s b),
		GSwizzleSet1 (Rep s) b,
		Rep (X s b) ~ GX (Rep s) b ) =>
		s -> b -> X s b
	x s b = to (gx (from s) b)

instance SwizzleSet1 (a, b, c) d where type X (a, b, c) d = (d, b, c)
