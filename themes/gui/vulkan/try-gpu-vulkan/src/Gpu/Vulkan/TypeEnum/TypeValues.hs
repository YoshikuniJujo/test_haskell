{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.TypeEnum.TypeValues (typeValues) where

import Language.Haskell.TH
import Data.Proxy
import Data.Char

typeValues :: String -> String -> [String] -> Q [Dec]
typeValues mdl tp elms =
	(\a b c -> a ++ b ++ c)	
		<$> ((: []) <$> mkType tp elms)
		<*> ((:)	<$> mkClass mdl tp
				<*> sequence (mkInstance mdl tp <$> elms))
		<*> sequence [
			sigFoo mdl tp,
			funD (mkName $ hd toLower tp ++ "ToType") (foo <$> elms) ]

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

mkInstance :: String -> String -> String -> Q Dec
mkInstance mdl tp ss =
	instanceD (pure []) (conT (mkName $ tp ++ "ToValue") `appT` promotedT (mkName ss)) [
		valD	(varP . mkName $ hd toLower tp ++ "ToValue")
			(normalB . conE . mkName $ mdl ++ "." ++ ss) []
		]

sigFoo :: String -> String -> Q Dec
sigFoo mdl tp = do
	t <- newName "t"
	a <- newName "a"
	tv <- kindedInvisTV t SpecifiedSpec (conT $ mkName tp)
	sigD (mkName $ hd toLower tp ++ "ToType")
		$ conT (mkName $ mdl ++ "." ++ tp)
			`arrT` ((forallT [tv]
				(cxt [conT (mkName $ tp ++ "ToValue") `appT` varT t])
				((conT ''Proxy `appT` varT t) `arrT` varT a))
		`arrT` varT a)

foo :: String -> Q Clause
foo nm = do
	f <- newName "f"
	clause
		[conP (mkName $ "E." ++ nm) [], varP f]
		(normalB $ varE f `appE`
			(conE 'Proxy `appTypeE` promotedT (mkName nm)))
		[]

arrT :: Q Type -> Q Type -> Q Type
t1 `arrT` t2 = arrowT `appT` t1 `appT` t2
