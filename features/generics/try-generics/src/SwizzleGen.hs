{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module SwizzleGen where

import Language.Haskell.TH

classSwizzle1 :: DecsQ
classSwizzle1 = (: []) <$> classD (cxt []) (mkName "Swizzle1") [plainTV $ mkName "a"] [] [
	typeX,
	funX
	]

typeX :: Q Dec
typeX = openTypeFamilyD (mkName "X") [plainTV $ mkName "a"] noSig Nothing

funX :: Q Dec
funX = sigD (mkName "x") $ varT (mkName "a") `arrT` (conT (mkName "X") `appT` varT (mkName "a"))

arrT :: TypeQ -> TypeQ -> TypeQ
t1 `arrT` t2 = arrowT `appT` t1 `appT` t2
