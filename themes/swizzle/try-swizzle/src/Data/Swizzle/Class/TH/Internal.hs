{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Swizzle.Class.TH.Internal (classSwizzle, instanceSwizzleTuple) where

import GHC.Generics
import Language.Haskell.TH
import Data.Bool
import Data.Char

classSwizzle :: Int -> DecsQ
classSwizzle i = sequence ((bool id (instanceGswizzle1K1 :) $ i == 1) [
		classGswizzle i,
		instanceGswizzleM1 i,
		instanceGswizzleProd i,
		instanceGswizzleProdProd i,
		classSwizzleClass i ])

instanceSwizzleTuple :: Int -> DecsQ
instanceSwizzleTuple n = (++)
	<$> (`instanceSwizzleTuple_` n) `mapM` [1 .. min n 26]
	<*> deriveGeneric n

classSwizzleClass :: Int -> Q Dec
classSwizzleClass i = newName "a" >>= \a ->
	classD (classSwizzleContext i a) (nameSwizzle i) [plainTV a] [] [
		typeX i a,
		sigX i a,
		defaultX i a,
		defaultFunX i ]

classSwizzleContext :: Int -> Name -> CxtQ
classSwizzleContext i a = case i of
	1 -> cxt []
	_ -> cxt [conT (nameSwizzle $ i - 1) `appT` varT a]

nameSwizzle :: Int -> Name
nameSwizzle = mkName . ("Swizzle" ++) . show

typeX :: Int -> Name -> Q Dec
typeX i a = openTypeFamilyD (nameXU i) [plainTV a] noSig Nothing

nameXU :: Int -> Name
nameXU = mkName . (: "") . toUpper . alphabet

sigX :: Int -> Name -> Q Dec
sigX i a = sigD (nameXL i) $ varT a `arrT` (conT (nameXU i) `appT` varT a)

nameXL :: Int -> Name
nameXL = mkName . (: "") . alphabet

defaultX :: Int -> Name -> Q Dec
defaultX i a = defaultSigD (nameXL i) . forallT [] (defaultXContext i a)
	$ varT a `arrT` (conT (nameXU i) `appT` varT a)

defaultXContext :: Int -> Name -> CxtQ
defaultXContext i a = cxt [
	conT ''Generic `appT` varT a,
	conT (nameGswizzle i) `appT` (conT ''Rep `appT` varT a),
	(conT (nameXU i) `appT` varT a) `eqT`
		(conT (nameGxU i) `appT` (conT ''Rep `appT` varT a))
	]

defaultFunX :: Int -> Q Dec
defaultFunX i = valD (varP $ nameXL i) (normalB
	$ infixE (Just . varE $ nameGxL i) (varE '(.)) (Just $ varE 'from)) []

arrT :: TypeQ -> TypeQ -> TypeQ
t1 `arrT` t2 = arrowT `appT` t1 `appT` t2

eqT :: TypeQ -> TypeQ -> TypeQ
t1 `eqT` t2 = equalityT `appT` t1 `appT` t2

classGswizzle :: Int -> Q Dec
classGswizzle i = newName "a" >>= \a -> classD (cxt [])
	(nameGswizzle i) [plainTV $ mkName "f"] [] [typeGx i, sigGx i a]

typeGx :: Int -> Q Dec
typeGx i = openTypeFamilyD (nameGxU i) [plainTV $ mkName "f"] noSig Nothing

sigGx :: Int -> Name -> Q Dec
sigGx i a = sigD (nameGxL i) $ (varT (mkName "f") `appT` varT a) `arrT`
	(conT (nameGxU i) `appT` varT (mkName "f"))

instanceGswizzle1K1 :: Q Dec
instanceGswizzle1K1 = newName "a" >>= \a ->
	instanceD (cxt []) (conT (mkName "GSwizzle1") `appT` k1ia a) [
		typeGxK1 a,
		funGxK1 a ]

k1ia :: Name -> TypeQ
k1ia a = conT ''K1 `appT` varT (mkName "i") `appT` varT a

typeGxK1 :: Name -> Q Dec
typeGxK1 a = tySynInstD
	$ tySynEqn Nothing (conT (mkName "GX") `appT` k1ia a) (varT a)

funGxK1 :: Name -> Q Dec
funGxK1 a = funD (mkName "gx") [
	clause [conP 'K1 [varP a]] (normalB $ varE a) [] ]

instanceGswizzleM1 :: Int -> Q Dec
instanceGswizzleM1 i = newName "a" >>= \a -> newName "c" >>= \c ->
	instanceD (cxtGswizzleM1 i a) (conT (nameGswizzle i) `appT` m1ica a c) [
		typeGxM1 i a c,
		funGxM1 i a
		]

cxtGswizzleM1 :: Int -> Name -> CxtQ
cxtGswizzleM1 i a = cxt [conT (nameGswizzle i) `appT` varT a]

m1ica :: Name -> Name -> TypeQ
m1ica a c = conT ''M1 `appT`
	varT (mkName "i") `appT` varT c `appT` varT a

typeGxM1 :: Int -> Name -> Name -> Q Dec
typeGxM1 i a c = tySynInstD $ tySynEqn Nothing
	(conT (nameGxU i) `appT` m1ica a c)
	(conT (nameGxU i) `appT` varT a)

funGxM1 :: Int -> Name -> Q Dec
funGxM1 i a = funD (nameGxL i) [clause [conP 'M1 [varP a]] (
	normalB $ varE (nameGxL i) `appE` varE a) []]

instanceGswizzleProd :: Int -> Q Dec
instanceGswizzleProd i = newName "a" >>= \a -> newName "b" >>= \b -> newName "c" >>= \c ->
	instanceD (cxtGswizzleProd i a b) (conT (nameGswizzle i) `appT` aProdB a b c) [
		typeGxProd i a b c,
		funGxProd i a b
		]

cxtGswizzleProd :: Int -> Name -> Name -> CxtQ
cxtGswizzleProd i a b = case i of
	1 -> cxt [conT (nameGswizzle 1) `appT` varT a]
	_ -> cxt [conT (nameGswizzle $ i - 1) `appT` varT b]

aProdB :: Name -> Name -> Name -> TypeQ
aProdB a b c =
	(conT ''M1 `appT` varT (mkName "i") `appT` varT c `appT` varT a) `prodT`
	varT b

infixr 9 `prodT`, `prodE`, `prodP`

prodT :: TypeQ -> TypeQ -> TypeQ
t1 `prodT` t2 = conT ''(:*:) `appT` t1 `appT` t2

prodE :: ExpQ -> ExpQ -> ExpQ
e1 `prodE` e2 = conE '(:*:) `appE` e1 `appE` e2

prodP :: PatQ -> PatQ -> PatQ
p1 `prodP` p2 = infixP p1 '(:*:) p2

typeGxProd :: Int -> Name -> Name -> Name -> Q Dec
typeGxProd i a b c = tySynInstD
	$ tySynEqn Nothing (conT (nameGxU i) `appT` aProdB a b c) (conT (nameGxxyU i) `appT` varT (nameAOrB i a b))

instanceGswizzleProdProd :: Int -> Q Dec
instanceGswizzleProdProd i = newName "a" >>= \a -> newName "b" >>= \b -> newName "c" >>= \c ->
	instanceD (cxtGswizzleProdProd i a b c) (conT (nameGswizzle i) `appT` aProdBProdCT' a b c) [
		typeGxProdProd i a b c,
		funGxProdProd i a b c
		]

cxtGswizzleProdProd :: Int -> Name -> Name -> Name -> CxtQ
cxtGswizzleProdProd i a b c = cxt [conT (nameGswizzle i) `appT` aProdBProdCT a b c]

typeGxProdProd :: Int -> Name -> Name -> Name -> Q Dec
typeGxProdProd i a b c = tySynInstD $ tySynEqn Nothing
	(conT (nameGxU i) `appT` aProdBProdCT' a b c)
	(conT (nameGxU i) `appT` aProdBProdCT a b c)

funGxProdProd :: Int -> Name -> Name -> Name -> Q Dec
funGxProdProd i a b c = funD (nameGxL i) [clause [aProdBProdCP' a b c] (normalB $ varE (nameGxL i) `appE` aProdBProdCE b c) []]

aProdBProdCT, aProdBProdCT' :: Name -> Name -> Name -> TypeQ
aProdBProdCT a b c = varT a `prodT` varT b `prodT` varT c
aProdBProdCT' a b c = (varT a `prodT` varT b) `prodT` varT c

aProdBProdCE :: Name -> Name -> ExpQ
aProdBProdCE b c = varE (mkName "a") `prodE` varE b `prodE` varE c

aProdBProdCP' :: Name -> Name -> Name -> PatQ
aProdBProdCP' a b c = (varP a `prodP` varP b) `prodP` varP c

nameGxxyU :: Int -> Name
nameGxxyU = \case 1 -> nameGxU 1; i -> nameGxU $ i - 1

nameAOrB :: Int -> Name -> Name -> Name
nameAOrB i a b = case i of 1 -> a; _ -> b

funGxProd :: Int -> Name -> Name -> Q Dec
funGxProd i a b = funD (nameGxL i) [
	clause [infixP (aOrWildP i a) '(:*:) (bOrWildP i b)]
		(normalB $ varE (nameGxxyL i) `appE` varE (nameAOrB i a b)) [] ]

aOrWildP :: Int -> Name -> PatQ
aOrWildP i a = case i of 1 -> varP a; _ -> wildP

bOrWildP :: Int -> Name -> PatQ
bOrWildP i b = case i of 1 -> wildP; _ -> varP b

nameGxxyL :: Int -> Name
nameGxxyL = \case 1 -> nameGxL 1; i -> nameGxL $ i - 1

nameGswizzle :: Int -> Name
nameGswizzle = mkName . ("GSwizzle" ++) . show

nameGxU :: Int -> Name
nameGxU i = mkName $ "G" ++ [toUpper $ alphabet i]

nameGxL :: Int -> Name
nameGxL i = mkName $ "g" ++ [alphabet i]

alphabet :: Int -> Char
alphabet i | i > 26 = error $ "no such alphabet: " ++ show i
alphabet i = (("xyz" ++ reverse ['a' .. 'w']) !!) $ subtract 1 i

vars :: [String]
vars = ((: "") <$> ['a' .. 'z']) ++ [ cs ++ [c] | cs <- vars, c <- ['a' .. 'z'] ]

instanceSwizzleTuple_ :: Int -> Int -> Q Dec
instanceSwizzleTuple_ i n = mapM (newName . (vars !!)) [0 .. n - 1] >>= \ns ->
	instanceD (cxt []) (conT (nameSwizzle i) `appT` tupT ns) [typeXFromTuple i ns]

typeXFromTuple :: Int -> [Name] -> Q Dec
typeXFromTuple i ns = tySynInstD $ tySynEqn Nothing (conT (nameXU i) `appT` tupT ns) (varT $ ns !! (i - 1))

tupT :: [Name] -> TypeQ
tupT ns = foldl appT (tupleT $ length ns) $ varT <$> ns

deriveGeneric :: Int -> DecsQ
deriveGeneric i = do
	t <- tupT =<< newNameAbc i
	isInstance ''Generic [t] >>= bool
		((: []) <$> standaloneDerivD (cxt [])
			(conT ''Generic `appT` pure t))
		(pure [])

newNameAbc :: Int -> Q [Name]
newNameAbc i = newName `mapM` take i vars
