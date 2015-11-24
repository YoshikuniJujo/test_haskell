{-# LANGUAGE TemplateHaskell #-}

module Database.SmplstSQLite3.Templates (newException, mkSqliteThrow) where

import Control.Applicative
import Control.Exception
import Data.Typeable
import Data.Char
import Language.Haskell.TH
import Foreign.C.Types

newException :: String -> DecQ
newException e =
	newtypeD (cxt []) (mkName e) []
		(normalC (mkName e) [strictType notStrict (conT ''String)])
		[''Typeable, ''Show]

sqliteThrowType :: DecQ
sqliteThrowType = sigD (mkName "sqliteThrow") .
	forallT [PlainTV $ mkName "a"] (cxt []) $
		conT ''String `arrT` conT ''CInt `arrT`
			(conT ''IO `appT` varT (mkName "a"))

mkSqliteThrow :: [Name] -> DecsQ
mkSqliteThrow ns = (:) <$> sqliteThrowType <*> ((: []) <$>
	funD (mkName "sqliteThrow") (map mc ns ++ [
		clause [varP $ mkName "em", wildP]
			(guardedB [(,) <$> normalG otgd <*> otbd]) [] ]))
	where
	mc n = clause [varP $ mkName "em", varP $ mkName "rc"]
		(guardedB [(,) <$> normalG (gd $ nameBase n) <*> bd n]) []
	gd n = infixE (Just . varE $ mkName "rc") (varE '(==))
		(Just . varE . mkName $ toLowerH n)
	bd n = infixE (Just $ varE 'throwIO) (varE '($)) . Just $
		conE n `appE` varE (mkName "em")
	otgd = varE 'otherwise
	otbd = infixE (Just $ varE 'throwIO) (varE '($)) . Just $
		conE (mkName "SQLITE_ERROR_OTHER") `appE` varE (mkName "em")

toLowerH :: String -> String
toLowerH (c : cs) = toLower c : cs
toLowerH _ = ""

infixr `arrT`
arrT :: TypeQ -> TypeQ -> TypeQ
arrT t1 t2 = arrowT `appT` t1 `appT` t2
