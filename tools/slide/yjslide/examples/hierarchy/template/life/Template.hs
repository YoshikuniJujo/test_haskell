{-# LANGUAGE TemplateHaskell #-}

module Template (
	newType,
	mkSomeDStr, instanceShow,
	mkSomePointer, instancePointer, instancePointerSome) where

import Language.Haskell.TH
import Control.Applicative
import Data.Char
import Data.Typeable
import Foreign.Ptr

-- import Life

newType :: (String -> TypeQ) -> String -> String -> String -> DecsQ
newType typ l s n
	| l == s = topType typ l n
	| otherwise = addType typ l s n

topType :: (String -> TypeQ) -> String -> String -> DecsQ
topType typ l n = sequence [
	mkData l n,
	instanceTop l n,
	mkToLifeTyp l n,
	mkToLife l n,
	mkFromLife l n,
	mkSomeData typ n,
	instanceSub l n ("Some" ++ n)
 ]

addType :: (String -> TypeQ) -> String -> String -> String -> DecsQ
addType typ l s n = sequence [
	mkData l n,
	instanceSub l s n,
	mkToLifeTyp l n,
	mkToLife l n,
	mkFromLife l n,
	mkSomeData typ n,
	instanceSub l n ("Some" ++ n)
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
	[''Typeable]
	where
	life = mkName l

mkSomeDStr :: String -> TypeQ
mkSomeDStr _ = conT $ mkName "String"

mkSomePointer :: String -> TypeQ
mkSomePointer n = conT ''Ptr `appT` conT (mkName n)

mkSomeData :: (String -> TypeQ) -> String -> DecQ
mkSomeData typ h = dataD
	(cxt [])
	(mkName $ "Some" ++ h)
	[]
	[normalC (mkName $ "Some" ++ h)
		[strictType notStrict $ typ $ "Some" ++ h]]
	[''Typeable]

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

instancePointer :: String -> DecQ
instancePointer n =
	instanceD (cxt []) (conT (mkName "Pointer") `appT` conT (mkName n)) [
		funD pointer $ (: []) $ flip
			(clause [conP (mkName n) [varP g]]) [] $
				normalB $ varE 'castPtr `appE`
					(varE pointer `appE` varE g),
		funD fromPointer $ (: []) $ flip (clause [varP p]) [] $
			normalB $ conE (mkName n) `appE`
				(varE fromPointer `appE` casted)
	 ]
	where
	[g, p] = map mkName ["g", "p"]
	pointer = mkName "pointer"
	fromPointer = mkName "fromPointer"
	casted = varE 'castPtr `appE` varE p `sigE`
		(conT ''Ptr `appT` conT (mkName $ "Some" ++ n))

instancePointerSome :: String -> DecQ
instancePointerSome n =
	instanceD (cxt []) (conT (mkName "Pointer") `appT` conT (mkName n)) [
		funD pointer $ (: []) $ flip (clause [conP nn [varP p]]) [] $
			normalB $ varE p,
		flip (valD $ varP fromPointer) [] $ normalB $ conE nn
	 ]
	where
	p = mkName "p"
	nn = mkName n
	pointer = mkName "pointer"
	fromPointer = mkName "fromPointer"
