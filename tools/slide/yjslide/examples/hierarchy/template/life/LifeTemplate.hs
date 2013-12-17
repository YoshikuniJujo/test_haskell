{-# LANGUAGE TemplateHaskell #-}

module LifeTemplate (mkTop, mkTopShow) where

import Language.Haskell.TH
import Control.Applicative
import Data.Typeable

mkTop :: String -> String -> DecsQ
mkTop c n = mapM ($ n) [mkData, mkClass c, instanceTop]

mkData :: String -> DecQ
mkData n = dataD (cxt []) (mkName $ "Some" ++ n) [] [
	forallC [PlainTV $ mkName "l"]
		(cxt [classP (mkName n) [varT $ mkName "l"]]) $
		normalC (mkName $ "Some" ++ n)
			[strictType notStrict $ varT $ mkName "l"]
 ] [''Typeable]

arrT :: TypeQ -> TypeQ -> TypeQ
arrT a b = arrowT `appT` a `appT` b

mkClass :: String -> String -> DecQ
mkClass c n = classD
	(cxt [classP (mkName c) [varT ln], classP ''Typeable [varT ln]])
	(mkName n) [PlainTV ln]
	[] [
		sigD to $ varT ln `arrT` conT sm,
		sigD from $ conT sm `arrT` (conT ''Maybe `appT` varT ln),
		valD (varP to) (normalB $ conE sm) [],
		funD from [clause [conP sm [varP ln]]
			(normalB $ varE (mkName "cast") `appE` varE ln) []
		 ]
	 ]
	where
	ln = mkName "l"
	to = mkName $ "to" ++ n
	from = mkName $ "from" ++ n
	sm = mkName $ "Some" ++ n

instanceTop :: String -> DecQ
instanceTop n = instanceD (cxt []) (conT nn `appT` conT sn) [
	valD (varP to) (normalB $ varE 'id) [],
	valD (varP from) (normalB $ conE 'Just) []
 ]
	where
	nn = mkName n
	sn = mkName $ "Some" ++ n
	to = mkName $ "to" ++ n
	from = mkName $ "from" ++ n

mkTopShow :: String -> DecsQ
mkTopShow n = (: []) <$> instanceD (cxt []) (conT ''Show `appT` conT sn) [
	funD 'show [clause [conP sn [varP $ mkName "l"]]
		(normalB $ varE 'show `appE` varE (mkName "l")) []]
 ]
	where
	sn = mkName $ "Some" ++ n
