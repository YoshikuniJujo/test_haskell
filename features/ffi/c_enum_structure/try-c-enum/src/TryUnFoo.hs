{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryUnFoo where

import Language.Haskell.TH

import Lib

unFooSig :: DecQ
unFooSig = unSig "Foo" ''Int

unFooFun :: DecQ
unFooFun = do
	f <- newName "f"
	funD (mkName "unFoo") [
		clause [conP ''Foo [varP f]] (normalB (varE f)) []
		]

unSig :: String -> Name -> DecQ
unSig en tp = sigD (mkName $ "un" ++ en) $ conT (mkName en) `arrT` conT tp

unFun :: String -> DecQ
unFun en = do
	x <- newName "x"
	funD (mkName $ "un" ++ en) [
		clause [conP (mkName en) [varP x]] (normalB (varE x)) []
		]

arrT :: TypeQ -> TypeQ -> TypeQ
t1 `arrT` t2 = arrowT `appT` t1 `appT` t2
