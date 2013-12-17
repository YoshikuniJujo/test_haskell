{-# LANGUAGE TemplateHaskell #-}

module Template (
	topType, topTypeShow,
	addType, addTypeShow) where

import Language.Haskell.TH
import Control.Applicative
import Data.Char
import Data.Typeable

-- import Life

topType :: String -> String -> DecsQ
topType l n = sequence [
	mkData l n,
	instanceTop l n,
	mkToLifeTyp l n,
	mkToLife l n,
	mkFromLife l n,
	mkSomeData n,
	instanceSub l n ("Some" ++ n)
 ]

topTypeShow :: String -> DecsQ
topTypeShow n = sequence [
	instanceShow n,
	instanceShowSome n
 ]

addType :: String -> String -> String -> DecsQ
addType l s n = sequence [
	mkData l n,
	instanceSub l s n,
	mkToLifeTyp l n,
	mkToLife l n,
	mkFromLife l n,
	mkSomeData n,
	instanceSub l n ("Some" ++ n)
 ]

addTypeShow :: String -> DecsQ
addTypeShow n = sequence [
	instanceShow n,
	instanceShowSome n
 ]

mkData :: String -> String -> DecQ
mkData l h = dataD
	(cxt [])
	(mkName h)
	[]
	[forallC [PlainTV $ mkName "h"]
		(cxt [classP life [varT $ mkName "h"]]) $
			normalC (mkName h)
				[strictType notStrict $ varT $ mkName "h"]]
	[mkName "Typeable"]
	where
	life = mkName l

mkSomeData :: String -> DecQ
mkSomeData h = dataD
	(cxt [])
	(mkName $ "Some" ++ h)
	[]
	[normalC (mkName $ "Some" ++ h)
		[strictType notStrict $ conT $ mkName "String"]]
	[mkName "Typeable"]

instanceShowSome :: String -> DecQ
instanceShowSome = instanceShow . ("Some" ++)

instanceShow :: String -> DecQ
instanceShow h = instanceD
	(cxt [])
	(conT (mkName "Show") `appT` conT (mkName h))
	[funD (mkName "show") [clause [conP (mkName h) [varP $ mkName "a"]] (normalB $
		varE (mkName "show") `appE` varE (mkName "a")) []]]

instanceTop :: String -> String -> DecQ
instanceTop l h = instanceD
	(cxt [])
	(conT life `appT` conT (mkName h))
	[]
	where
	life = mkName l

instanceSub :: String -> String -> String -> DecQ
instanceSub l (h : t) n = instanceD
	(cxt [])
	(conT life `appT` conT (mkName n)) [
		valD (varP tol) (normalB $ varE to) [],
		valD (varP froml) (normalB $ varE from) []
	 ]
	where
	s = toLower h : t
	life = mkName l
	tol = mkName $ "to" ++ l
	froml = mkName $ "from" ++ l
	to = mkName $ s ++ "To" ++ l
	from = mkName $ s ++ "From" ++ l

arrT :: TypeQ -> TypeQ -> TypeQ
arrT a b = arrowT `appT` a `appT` b

mkToLifeTyp :: String -> String -> DecQ
mkToLifeTyp l (h : t) =
	sigD to $ forallT [PlainTV $ mkName "a"]
		(cxt [classP life [varT $ mkName "a"]]) $
			varT (mkName "a") `arrT` conT sm
	where
	n = toLower h : t
	to = mkName $ n ++ "To" ++ l
	life = mkName l
	sm = mkName $ "Some" ++ l

mkToLife :: String -> String -> DecQ
mkToLife l nt@(h : t) =
	funD to
	[clause [] (normalB $ uInfixE (varE tol) (varE $ mkName ".") (conE $ mkName nt)) []]
	where
	n = toLower h : t
	to = mkName $ n ++ "To" ++ l
	tol = mkName $ "to" ++ l

mkFromLife :: String -> String -> DecQ
mkFromLife l nt@(h : t) =
	funD from
	[clause [varP $ mkName "l"] (normalB $ doE [
		bindS (conP (mkName nt) $ [varP $ mkName "h"]) $
			varE froml `appE` varE (mkName "l"),
		noBindS $ varE 'cast `appE` varE (mkName "h")
	 ]) []]
	where
	n = toLower h : t
	from = mkName $ n ++ "From" ++ l
	froml = mkName $ "from" ++ l
