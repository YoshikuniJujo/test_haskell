{-# LANGUAGE TemplateHaskell #-}

import Control.Applicative
import Control.Exception
import Data.Typeable
import Language.Haskell.TH

setx :: DecsQ
setx = sequence [
	valD (varP $ mkName "x") (normalB . litE $ integerL 888) []
	]

myException :: DecsQ
myException = sequence [
	newtypeD (cxt [])
		(mkName "MyException") []
		(normalC (mkName "MyException")
			[strictType notStrict (conT ''String)])
		[''Typeable, ''Show],
	instanceD (cxt [])
		(conT ''Exception `appT` conT (mkName "MyException"))
		[]
	]

humanError :: DecsQ
humanError = sequence [
	dataD (cxt []) he []
		[forallC [PlainTV e] (cxt [classP ''Exception [varT e]]) $
			normalC he [strictType notStrict (varT e)]]
		[''Typeable],
	instanceD (cxt []) (conT ''Show `appT` conT he)
		[funD 'show
			[clause [conP he [varP e]]
				(normalB $ varE 'show `appE` varE e) []]],
	instanceD (cxt []) (conT ''Exception `appT` conT he) [],
	sigD toEx . forallT [PlainTV e] (cxt [classP ''Exception [varT e]]) $
		varT e `arrT` conT ''SomeException,
	valD (varP toEx)
		(normalB $ infixE
			(Just $ varE 'toException) (varE '(.)) (Just $ conE he))
		[],
	sigD fromEx . forallT [PlainTV e] (cxt [classP ''Exception [varT e]]) $
		conT ''SomeException `arrT` (conT ''Maybe `appT` varT e),
	funD fromEx [clause
		[varP se]
		(normalB $ doE [
			bindS (conP he [varP e])
				(varE 'fromException `appE` varE se),
			noBindS $ varE 'cast `appE` varE e])
		[]]
	]
	where
	he = mkName "HumanError"
	toEx = mkName "humanErrorToException"
	fromEx = mkName "humanErrorFromException"
	e = mkName "e"
	se = mkName "se"

infixr `arrT`
arrT :: TypeQ -> TypeQ -> TypeQ
arrT t1 t2 = arrowT `appT` t1 `appT` t2
