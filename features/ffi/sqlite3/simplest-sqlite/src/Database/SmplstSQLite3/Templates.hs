{-# LANGUAGE TemplateHaskell #-}

module Database.SmplstSQLite3.Templates (newException, mkSqliteThrow) where

import Control.Exception
import Data.Typeable
import Data.Char
import Language.Haskell.TH
import Foreign.C.Types

myNotStrict :: Q Strict
myNotStrict = bang noSourceUnpackedness noSourceStrictness

newException :: String -> DecQ
newException e =
	newtypeD (cxt []) (mkName e) [] Nothing
		(normalC (mkName e) [bangType myNotStrict (conT ''String)]) [
			derivClause Nothing [conT ''Typeable, conT ''Show] ]

sqliteThrowType :: DecQ
sqliteThrowType = sigD (mkName "sqliteThrow") .
	forallT [plainTV $ mkName "a"] (cxt []) $
		conT ''String `arrT` conT ''CInt `arrT`
			(conT ''IO `appT` varT (mkName "a"))

mkSqliteThrow :: [Name] -> DecsQ
mkSqliteThrow nms = (:)
	<$> sqliteThrowType
	<*> ((: []) <$> funD (mkName "sqliteThrow") [mc nms])
	where
	mc ns = clause [varP $ mkName "em", varP $ mkName "rc"]
		(guardedB $
			map gd1 ns ++ [(,) <$> normalG otgd <*> otbd])
		[]
	gd1 n = (,) <$> normalG (gd $ nameBase n) <*> bd n
	gd n = infixE (Just . varE $ mkName "rc") (varE '(==))
		(Just . varE . mkName $ toLowerH n)
	bd n = infixE (Just $ varE 'throwIO) (varE '($)) . Just $
		conE n `appE` varE (mkName "em")
	otgd = varE 'otherwise
	otbd = infixE (Just $ varE 'throwIO) (varE '($)) . Just $
		conE (mkName "SQLITE_ERROR_OTHER") `appE` varE (mkName "rc") `appE` varE (mkName "em")

toLowerH :: String -> String
toLowerH (c : cs) = toLower c : cs
toLowerH _ = ""

infixr `arrT`
arrT :: TypeQ -> TypeQ -> TypeQ
arrT t1 t2 = arrowT `appT` t1 `appT` t2
