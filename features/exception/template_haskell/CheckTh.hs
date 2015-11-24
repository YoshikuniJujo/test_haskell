{-# LANGUAGE TemplateHaskell #-}

module CheckTh (
	myException, humanError, manError, nomanError, ghostError,
	exceptionTree, mkName) where

import Control.Applicative
import Control.Exception
import Data.Typeable
import Data.Tree
import Data.Char
import Language.Haskell.TH

myException :: DecsQ
myException = sequence [
	newtypeD (cxt [])
		(mkName "MyException") []
		(normalC (mkName "MyException")
			[strictType notStrict (conT ''String)])
		[''Typeable, ''Show],
	defInstException $ mkName "MyException"
	]

humanError :: DecsQ
humanError = (:) <$> defInstException he <*> exceptionContainer he
	where he = mkName "HumanError"

manError :: DecsQ
manError = (:) <$> instException (mkName "HumanError") (mkName "ManError") <*>
	sequence [
		newtypeD (cxt [])
			(mkName "ManError") []
			(normalC (mkName "ManError")
				[strictType notStrict (conT ''String)])
			[''Typeable, ''Show] ]

nomanError :: DecsQ
nomanError = (:)
	<$> instException (mkName "HumanError") (mkName "NomanError")
	<*> exceptionContainer (mkName "NomanError")

ghostError :: DecsQ
ghostError = (: []) <$> instException (mkName "NomanError") (mkName "GhostError")

exceptionContainer :: Name -> DecsQ
exceptionContainer ec = sequence [
	dataD (cxt []) he []
		[forallC [PlainTV e] (cxt [classP ''Exception [varT e]]) $
			normalC he [strictType notStrict (varT e)]]
		[''Typeable],
	instanceD (cxt []) (conT ''Show `appT` conT he)
		[funD 'showsPrec
			[clause [varP d, conP he [varP e]]
				(normalB $ varE 'showsPrec `appE` varE d `appE` varE e) []]],
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
	he = ec
	ec' = toLowerH $ nameBase ec
	toEx = mkName $ ec' ++ "ToException"
	fromEx = mkName $ ec' ++ "FromException"
	e = mkName "e"
	se = mkName "se"
	d = mkName "d"

toLowerH :: String -> String
toLowerH (c : cs) = toLower c : cs

defInstException :: Name -> DecQ
defInstException e = instanceD (cxt []) (conT ''Exception `appT` conT e) []

infixr `arrT`
arrT :: TypeQ -> TypeQ -> TypeQ
arrT t1 t2 = arrowT `appT` t1 `appT` t2

instException :: Name -> Name -> DecQ
instException ec e = instanceD (cxt [])
	(conT ''Exception `appT` conT e) [
		valD (varP $ mkName "toException") (normalB $ varE te) [],
		valD (varP $ mkName "fromException") (normalB $ varE fe) [] ]
	where
	ec' = toLowerH $ nameBase ec
	te = mkName $ ec' ++ "ToException"
	fe = mkName $ ec' ++ "FromException"

exception1 :: Maybe Name -> Name -> Bool -> DecsQ
exception1 mc e c = (:)
	<$> maybe (defInstException e) (`instException` e) mc
	<*> if c then exceptionContainer e else return []

exceptionTree :: Maybe Name -> Tree Name -> DecsQ
exceptionTree mc (Node e []) = exception1 mc e False
exceptionTree mc (Node e es) = (concat .) . (:)
	<$> exception1 mc e True
	<*> mapM (exceptionTree $ Just e) es
