{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module SwizzleGen where

import GHC.Generics
import Language.Haskell.TH

classSwizzle1 :: DecsQ
classSwizzle1 = (: []) <$> classD (cxt []) (mkName "Swizzle1") [plainTV $ mkName "a"] [] [
	typeX,
	sigX,
	defaultX,
	defaultFunX
	]

typeX :: Q Dec
typeX = openTypeFamilyD (mkName "X") [plainTV $ mkName "a"] noSig Nothing

sigX :: Q Dec
sigX = sigD (mkName "x") $ varT (mkName "a") `arrT` (conT (mkName "X") `appT` varT (mkName "a"))

defaultX :: Q Dec
defaultX = defaultSigD (mkName "x") . forallT [] defaultXContext
	$ varT (mkName "a") `arrT` (conT (mkName "X") `appT` varT (mkName "a"))

defaultXContext :: CxtQ
defaultXContext = cxt [
	conT ''Generic `appT` varT (mkName "a"),
	conT (mkName "GSwizzle1") `appT` (conT ''Rep `appT` varT (mkName "a")),
	(conT (mkName "X") `appT` varT (mkName "a")) `eqT`
		(conT (mkName "GX") `appT` (conT ''Rep `appT` varT (mkName "a")))
	]

defaultFunX :: Q Dec
defaultFunX = valD (varP $ mkName "x") (normalB $ infixE (Just . varE $ mkName "gx") (varE '(.)) (Just $ varE 'from)) []

arrT :: TypeQ -> TypeQ -> TypeQ
t1 `arrT` t2 = arrowT `appT` t1 `appT` t2

eqT :: TypeQ -> TypeQ -> TypeQ
t1 `eqT` t2 = equalityT `appT` t1 `appT` t2

classGSwizzle1 :: DecsQ
classGSwizzle1 = (: []) <$> classD (cxt []) (mkName "GSwizzle1") [plainTV $ mkName "f"] [] [
	typeGx,
	sigGx
	]

typeGx :: Q Dec
typeGx = openTypeFamilyD (mkName "GX") [plainTV $ mkName "f"] noSig Nothing

sigGx :: Q Dec
sigGx = sigD (mkName "gx") $ (varT (mkName "f") `appT` varT (mkName "a")) `arrT`
	(conT (mkName "GX") `appT` varT (mkName "f"))

instanceGswizzle1M1 :: DecsQ
instanceGswizzle1M1 = (: []) <$> instanceD cxtGswizzle1M1 (conT (mkName "GSwizzle1") `appT` m1ica) [
	typeGxM1,
	funGxM1
	]

cxtGswizzle1M1 :: CxtQ
cxtGswizzle1M1 = cxt [conT (mkName "GSwizzle1") `appT` varT (mkName "a")]

m1ica :: TypeQ
m1ica = conT (mkName "M1") `appT`
	varT (mkName "i") `appT` varT (mkName "c") `appT` varT (mkName "a")

typeGxM1 :: Q Dec
typeGxM1 = tySynInstD $ tySynEqn Nothing
	(conT (mkName "GX") `appT` m1ica)
	(conT (mkName "GX") `appT` varT (mkName "a"))

funGxM1 :: Q Dec
funGxM1 = funD (mkName "gx") [clause [conP (mkName "M1") [varP $ mkName "a"]] (
	normalB $ varE (mkName "gx") `appE` varE (mkName "a") ) []]

instanceGswizzle1K1 :: DecsQ
instanceGswizzle1K1 = (: []) <$> instanceD (cxt []) (conT (mkName "GSwizzle1") `appT` k1ia) [
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

instanceGswizzle1Prod :: DecsQ
instanceGswizzle1Prod = (: []) <$> instanceD cxtGswizzle1M1 (conT (mkName "GSwizzle1") `appT` aProdB) [
	typeGxProd,
	funGxProd
	]

aProdB :: TypeQ
aProdB = conT (mkName ":*:") `appT` varT (mkName "a") `appT` varT (mkName "_b")

typeGxProd :: Q Dec
typeGxProd = tySynInstD
	$ tySynEqn Nothing (conT (mkName "GX") `appT` aProdB) (conT (mkName "GX") `appT` varT (mkName "a"))

funGxProd :: Q Dec
funGxProd = funD (mkName "gx") [
	clause [infixP (varP $ mkName "x_") (mkName ":*:") wildP]
		(normalB $ varE (mkName "gx") `appE` varE (mkName "x_")) [] ]
