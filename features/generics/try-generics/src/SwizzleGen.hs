{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module SwizzleGen where

import Language.Haskell.TH

classSwizzle1 :: DecsQ
classSwizzle1 = (: []) <$> classD (cxt []) (mkName "Swizzle1") [plainTV $ mkName "a"] [] [
	typeX,
	sigX
	]

typeX :: Q Dec
typeX = openTypeFamilyD (mkName "X") [plainTV $ mkName "a"] noSig Nothing

sigX :: Q Dec
sigX = sigD (mkName "x") $ varT (mkName "a") `arrT` (conT (mkName "X") `appT` varT (mkName "a"))

arrT :: TypeQ -> TypeQ -> TypeQ
t1 `arrT` t2 = arrowT `appT` t1 `appT` t2

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
	typeGxM1
--	funGxM1
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

-- funGxM1 :: Q Dec
-- funGxM1 = ...
