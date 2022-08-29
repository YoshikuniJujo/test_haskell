{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CEnum.SampleType.Th where

import Language.Haskell.TH
import Data.Char

mkType :: String -> [String] -> Q Dec
mkType tp elms = dataD (pure []) (mkName tp) [] Nothing
	((`normalC` []) . mkName <$> elms) [derivClause Nothing [conT ''Show]]

mkClass :: String -> String -> Q Dec
mkClass mdl tp = do
	t <- newName "t"
	classD (cxt []) (mkName $ tp ++ "ToValue")
		[kindedTV t (ConT $ mkName tp)]
		[]
		[sigD (mkName $ hd toLower tp ++ "ToValue") . conT . mkName $ mdl ++ "." ++ tp]

hd :: (a -> a) -> [a] -> [a]
hd _ [] = []
hd f (x : xs) = f x : xs

mkInstance :: String -> Q Dec
mkInstance ss =
	instanceD (pure []) (conT (mkName "EnumSampleToValue") `appT` promotedT (mkName ss)) [
		valD (varP $ mkName "enumSampleToValue") (normalB . conE . mkName $ "E." ++ ss) []
		]

sigFoo :: Q Dec
sigFoo = do
	t <- newName "t"
	a <- newName "a"
	tv <- kindedInvisTV t SpecifiedSpec (conT $ mkName "EnumSample")
	sigD (mkName "enumSampleToType")
		$ conT (mkName "E.EnumSample")
			`arrT` ((forallT [tv]
				(cxt [conT (mkName "EnumSampleToValue") `appT` varT t])
				((conT (mkName "Proxy") `appT` varT t) `arrT` varT a))
		`arrT` varT a)

foo :: String -> Q Clause
foo nm = do
	f <- newName "f"
	clause
		[conP (mkName $ "E." ++ nm) [], varP f]
		(normalB $ varE f `appE`
			(conE (mkName "Proxy") `appTypeE` promotedT (mkName nm)))
		[]

arrT :: Q Type -> Q Type -> Q Type
t1 `arrT` t2 = arrowT `appT` t1 `appT` t2
