{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module SwizzleGen where

import GHC.Generics
import Language.Haskell.TH
import Data.Bool
import Data.Char

classSwizzle :: Int -> DecsQ
classSwizzle i = sequence
	$ (bool id (instanceGswizzle1K1 :) $ i == 1) [
		classGswizzle i,
		instanceGswizzleM1 i,
		instanceGswizzleProd i,
		classSwizzleClass i ]

classSwizzleClass :: Int -> Q Dec
classSwizzleClass i = classD (cxt []) (nameSwizzle i) [plainTV $ mkName "a"] [] [
	typeX i,
	sigX i,
	defaultX i,
	defaultFunX i
	]

nameSwizzle :: Int -> Name
nameSwizzle = mkName . ("Swizzle" ++) . show

typeX :: Int -> Q Dec
typeX i = openTypeFamilyD (nameXU i) [plainTV $ mkName "a"] noSig Nothing

nameXU :: Int -> Name
nameXU = mkName . (: "") . toUpper . alphabet

sigX :: Int -> Q Dec
sigX i = sigD (nameXL i) $ varT (mkName "a") `arrT` (conT (nameXU i) `appT` varT (mkName "a"))

nameXL :: Int -> Name
nameXL = mkName . (: "") . alphabet

defaultX :: Int -> Q Dec
defaultX i = defaultSigD (nameXL i) . forallT [] (defaultXContext i)
	$ varT (mkName "a") `arrT` (conT (nameXU i) `appT` varT (mkName "a"))

defaultXContext :: Int -> CxtQ
defaultXContext i = cxt [
	conT ''Generic `appT` varT (mkName "a"),
	conT (nameGswizzle i) `appT` (conT ''Rep `appT` varT (mkName "a")),
	(conT (nameXU i) `appT` varT (mkName "a")) `eqT`
		(conT (nameGxU i) `appT` (conT ''Rep `appT` varT (mkName "a")))
	]

defaultFunX :: Int -> Q Dec
defaultFunX i = valD (varP $ nameXL i) (normalB
	$ infixE (Just . varE $ nameGxL i) (varE '(.)) (Just $ varE 'from)) []

arrT :: TypeQ -> TypeQ -> TypeQ
t1 `arrT` t2 = arrowT `appT` t1 `appT` t2

eqT :: TypeQ -> TypeQ -> TypeQ
t1 `eqT` t2 = equalityT `appT` t1 `appT` t2

classGswizzle :: Int -> Q Dec
classGswizzle i = classD (cxt [])
	(nameGswizzle i) [plainTV $ mkName "f"] [] [typeGx i, sigGx i]

typeGx :: Int -> Q Dec
typeGx i = openTypeFamilyD (nameGxU i) [plainTV $ mkName "f"] noSig Nothing

sigGx :: Int -> Q Dec
sigGx i = sigD (nameGxL i) $ (varT (mkName "f") `appT` varT (mkName "a")) `arrT`
	(conT (nameGxU i) `appT` varT (mkName "f"))

instanceGswizzle1K1 :: Q Dec
instanceGswizzle1K1 = instanceD (cxt []) (conT (mkName "GSwizzle1") `appT` k1ia) [
	typeGxK1,
	funGxK1
	]

k1ia :: TypeQ
k1ia = conT (mkName "K1") `appT` varT (mkName "i") `appT` varT (mkName "a")

typeGxK1 :: Q Dec
typeGxK1 = tySynInstD
	$ tySynEqn Nothing (conT (mkName "GX") `appT` k1ia) (varT $ mkName "a")

funGxK1 :: Q Dec
funGxK1 = funD (mkName "gx") [
	clause [conP (mkName "K1") [varP $ mkName "a"]]
		(normalB . varE $ mkName "a") [] ]

instanceGswizzleM1 :: Int -> Q Dec
instanceGswizzleM1 i = instanceD (cxtGswizzleM1 i) (conT (nameGswizzle i) `appT` m1ica) [
	typeGxM1 i,
	funGxM1 i
	]

cxtGswizzleM1 :: Int -> CxtQ
cxtGswizzleM1 i = cxt [conT (nameGswizzle i) `appT` varT (mkName "a")]

m1ica :: TypeQ
m1ica = conT (mkName "M1") `appT`
	varT (mkName "i") `appT` varT (mkName "c") `appT` varT (mkName "a")

typeGxM1 :: Int -> Q Dec
typeGxM1 i = tySynInstD $ tySynEqn Nothing
	(conT (nameGxU i) `appT` m1ica)
	(conT (nameGxU i) `appT` varT (mkName "a"))

funGxM1 :: Int -> Q Dec
funGxM1 i = funD (nameGxL i) [clause [conP (mkName "M1") [varP $ mkName "a"]] (
	normalB $ varE (nameGxL i) `appE` varE (mkName "a") ) []]

instanceGswizzleProd :: Int -> Q Dec
instanceGswizzleProd i = instanceD (cxtGswizzleProd i) (conT (nameGswizzle i) `appT` aProdB) [
	typeGxProd i,
	funGxProd i
	]

cxtGswizzleProd :: Int -> CxtQ
cxtGswizzleProd = \case
	1 -> cxt [conT (nameGswizzle 1) `appT` varT (mkName "a")]
	i -> cxt [conT (nameGswizzle $ i - 1) `appT` varT (mkName "b")]

aProdB :: TypeQ
aProdB = conT (mkName ":*:") `appT` varT (mkName "a") `appT` varT (mkName "b")

typeGxProd :: Int -> Q Dec
typeGxProd i = tySynInstD
	$ tySynEqn Nothing (conT (nameGxU i) `appT` aProdB) (conT (nameGxxyU i) `appT` varT (nameAOrB i))

nameGxxyU :: Int -> Name
nameGxxyU = \case 1 -> nameGxU 1; i -> nameGxU $ i - 1

nameAOrB :: Int -> Name
nameAOrB = \case 1 -> mkName "a"; _ -> mkName "b"

funGxProd :: Int -> Q Dec
funGxProd i = funD (nameGxL i) [
	clause [infixP (aOrWildP i) (mkName ":*:") (bOrWildP i)]
		(normalB $ varE (nameGxxyL i) `appE` varE (nameAOrB i)) [] ]

aOrWildP :: Int -> PatQ
aOrWildP = \case 1 -> varP $ mkName "a"; _ -> wildP

bOrWildP :: Int -> PatQ
bOrWildP = \case 1 -> wildP; _ -> varP $ mkName "b"

nameGxxyL :: Int -> Name
nameGxxyL = \case 1 -> nameGxL 1; i -> nameGxL $ i - 1

nameGswizzle :: Int -> Name
nameGswizzle = mkName . ("GSwizzle" ++) . show

nameGxU :: Int -> Name
nameGxU i = mkName $ "G" ++ [toUpper $ alphabet i]

nameGxL :: Int -> Name
nameGxL i = mkName $ "g" ++ [alphabet i]

alphabet :: Int -> Char
alphabet = (("xyz" ++ reverse ['a' .. 'w']) !!) . subtract 1
