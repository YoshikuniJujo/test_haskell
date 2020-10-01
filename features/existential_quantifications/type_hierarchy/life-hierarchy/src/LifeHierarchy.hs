{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module LifeHierarchy where

import Data.Typeable
import Data.Char
import Language.Haskell.TH

import SomeLife

data LifeHierarchy
	= LifeNode String [LifeHierarchy]
	| LifeType Name
	deriving Show

lifeHierarchy :: Maybe Name -> LifeHierarchy -> DecsQ
lifeHierarchy Nothing (LifeNode l ls) = (\x y z -> x : y ++ concat z)
	<$> defInstLife (mkName l) <*> lifeContainer (mkName l) <*> lifeHierarchy (Just $ mkName l) `mapM` ls
lifeHierarchy (Just c) (LifeNode l ls) = (\x y z -> x : y ++ concat z)
	<$> instLife c (mkName l) <*> lifeContainer (mkName l) <*> lifeHierarchy (Just $ mkName l) `mapM` ls
lifeHierarchy Nothing (LifeType l) = (: []) <$> defInstLife l
lifeHierarchy (Just c) (LifeType l) = (: []) <$> instLife  c l

lifeContainer :: Name -> DecsQ
lifeContainer ec = sequence [
	do	e <- newName "e"
		dataD (cxt []) ec [] Nothing
			[forallC [PlainTV e] (cxt [myClassP ''Life [varT e]])
				$ normalC ec [bangType myNotStrict (varT e)]]
			[derivClause Nothing [conT ''Typeable]],
	do	e <- newName "e"
		d <- newName "d"
		instanceD (cxt []) (conT ''Show `appT` conT ec)
			[funD 'showsPrec
				[clause [varP d, conP ec [varP e]]
					(normalB $ varE 'showsPrec `appE` varE d `appE` varE e) []]],
	do	e <- newName "e"
		sigD toL . forallT [PlainTV e] (cxt [myClassP ''Life [varT e]])
			$ varT e `arrT` conT ''SomeLife,
	valD (varP toL)
		(normalB $ infixE
			(Just $ varE 'toLife) (varE '(.)) (Just $ conE ec))
		[],
	do	e <- newName "e"
		sigD fromL . forallT [PlainTV e] (cxt [myClassP ''Life [varT e]])
			$ conT ''SomeLife `arrT` (conT ''Maybe `appT` varT e),
	do	e <- newName "e"
		se <- newName "se"
		funD fromL [clause
			[varP se]
			(normalB $ doE [
				bindS (conP ec [varP e])
					(varE 'fromLife `appE` varE se),
				noBindS $ varE 'cast `appE` varE e])
			[]]
	]
	where
	ec' = toLower `appHead` nameBase ec
	toL = mkName $ ec' ++ "ToLife"
	fromL = mkName $ ec' ++ "FromLife"

infixr `arrT`
arrT :: TypeQ -> TypeQ -> TypeQ
t1 `arrT` t2 = arrowT `appT` t1 `appT` t2

myClassP :: Name -> [Q Type] -> Q Pred
myClassP cla tys = do
	tys1 <- sequence tys
	pure $ foldl AppT (ConT cla) tys1

myNotStrict :: Q Strict
myNotStrict = bang noSourceUnpackedness noSourceStrictness

defInstLife :: Name -> DecQ
defInstLife e = instanceD (cxt []) (conT ''Life `appT` conT e) []

instLife :: Name -> Name -> DecQ
instLife (appHead toLower . nameBase -> ec) e = instanceD (cxt [])
	(conT ''Life `appT` conT e) [
		valD (varP $ mkName "toLife") (normalB $ varE te) [],
		valD (varP $ mkName "fromLife") (normalB $ varE fe) [] ]
	where
	te = mkName $ ec ++ "ToLife"
	fe = mkName $ ec ++ "FromLife"

appHead :: (a -> a) -> [a] -> [a]
appHead _ [] = []
appHead f (h : t) = f h : t
