{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.SwizzleSet.Class.TH (
	classSwizzle, instanceSwizzleTuple
	) where

import GHC.Generics
import Language.Haskell.TH hiding (Type)
import Data.Kind
import Data.Bool

import Template.Tools

classSwizzle :: Int -> DecsQ
classSwizzle n =
	sequence $ bool id (\x -> (instanceGswizzle1K1 : x)) (n == 1) [
--	sequence $ bool id (\x -> (instanceGswizzle1K1 : instanceGswizzleProdProd 1 : x)) (n == 1) [
	classGswizzle n,
	instanceGswizzleM1 n,
	instanceGswizzleProd n,
	prodProd n,
--	instanceGswizzleProdProd n,
	classSwizzleClass n ]

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
	newName "b" >>= \b ->
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
	newName "b" >>= \b -> newName "g" >>= \g ->
	newName "s" >>= \s -> newName "t" >>= \t -> newName "v" >>= \v ->
	instanceD (cxtGswizzleProd n a g b)
		(instanceHeadProd n i c a g b)
		[typeGxProd n i c a g b, funGxProd n v s t]

cxtGswizzleProd :: Int -> Name -> Name -> Name -> CxtQ
cxtGswizzleProd n a g b = case n of
	1 -> cxt [conT (nameGswizzle 1) `appT` varT a `appT` varT b]
	_ -> cxt [conT (nameGswizzle $ n - 1) `appT` varT g `appT` varT b]

instanceHeadProd :: Int ->
	Name -> Name -> Name -> Name -> Name -> TypeQ
instanceHeadProd n i c a g b = case n of
	1 -> conT (nameGswizzle n) `appT` aProdG i c a g `appT` varT b
	_ -> conT (nameGswizzle n) `appT` aProdG i c a g `appT` varT b

typeGxProd :: Int -> Name -> Name -> Name -> Name -> Name -> Q Dec
typeGxProd n i c a g b = tySynInstD $ tySynEqn Nothing
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

classSwizzleClass :: Int -> Q Dec
classSwizzleClass n =
	newName "s" >>= \ta -> newName "b" >>= \b ->
	newName "s" >>= \fs -> newName "v" >>= \v ->
	classD (cxt []) (nameSwizzle n) [plainTV ta, plainTV b] [] [
		typeX n ta b,
		sigX n ta b,
		defaultX n ta b,
		defaultFunX n fs v ]

typeX :: Int -> Name -> Name -> Q Dec
typeX i s b = openTypeFamilyD (nameXU i) [plainTV s, plainTV b] noSig Nothing

sigX :: Int -> Name -> Name -> Q Dec
sigX i a b = sigD (nameXL i) $
	varT a `arrT` varT b `arrT` (conT (nameXU i) `appT` varT a `appT` varT b)

instanceSwizzleTuple :: Int -> DecsQ
instanceSwizzleTuple n = (++)
	<$> (`instanceSwizzleTuple_` n) `mapM` [1 .. min n 26]
	<*> deriveGeneric n

instanceSwizzleTuple_ :: Int -> Int -> Q Dec
instanceSwizzleTuple_ i n =
	mapM (newName . (vars !!)) [0 .. n - 1] >>= \ns -> newName "x" >>= \x ->
	instanceD (cxt [])
		(conT (nameSwizzle i) `appT` tupT ns `appT` varT x)
		[typeXFromTuple i ns x]

typeXFromTuple :: Int -> [Name] -> Name -> Q Dec
typeXFromTuple i ns x =
	let	ns' = setX ns (i - 1) x in
	tySynInstD $
		tySynEqn Nothing (conT (nameXU i) `appT` tupT ns `appT` varT x) (tupT ns')

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
	(normalB $
		varE 'to `appE` (varE (nameGxL i)
			`appE` (varE 'from `appE` varE s)
			`appE` varE v
			))
	[]]

alphabet :: Int -> Char
alphabet i | i > 26 = error $ "no such alphabet: " ++ show i
alphabet i = (("xyz" ++ reverse ['a' .. 'w']) !!) $ subtract 1 i

prodProd :: Int -> DecQ
prodProd n = newName "a" >>= \a -> newName "b" >>= \b -> newName "c" >>= \c ->
	newName "v" >>= \v ->
	newName "x" >>= \x -> newName "y" >>= \y -> newName "z" >>= \z ->
	newName "v" >>= \vf ->
	instanceD
		(cxt [
			conT (nameGswizzle n) `appT`
				(varT a `prodT`
					(varT b `prodT` varT c)) `appT` varT v,
			conT ''Push `appT`
				(conT (nameGxU n) `appT`
					(varT a `prodT` (varT b `prodT` varT c))
						`appT` varT v)
			])
		(	conT (nameGswizzle n) `appT` ((varT a `prodT` varT b) `prodT` varT c) `appT` varT v
			)
		[
			tySynInstD (tySynEqn Nothing
				(conT (nameGxU n) `appT`
					((varT a `prodT` varT b) `prodT` varT c) `appT`
					varT v)
				(conT ''P `appT`
					(conT (nameGxU n) `appT` (varT a `prodT` (varT b `prodT` varT c)) `appT` varT v))),
			funD (nameGxL n) [clause
				[	(varP x `prodP` varP y) `prodP` varP z,
					varP vf
					]
				(normalB $ varE 'push `appE`
					(varE (nameGxL n) `appE` (varE x `prodE` (varE y `prodE` varE z)) `appE` varE vf)
					)
				[]]
			]

class Push x where type P x :: k -> Data.Kind.Type; push :: x a -> P x a

instance Push (a :*: (b :*: c)) where
	type P (a :*: (b :*: c)) = (a :*: b) :*: c
	push (x :*: (y :*: z)) = (x :*: y) :*: z
