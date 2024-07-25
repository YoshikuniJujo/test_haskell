{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad
import Data.Maybe

import Hason
import Ex4.Parser

main :: IO ()
main = print . (evaluate =<<) . parse =<< readFile "sample_hs/Sample.hs"

evaluate :: ([[String]], Mdl) -> Either String Hason
evaluate rslt = do
	(e, ds) <- checkModule =<< checkPragmas rslt
	checkTypeDecls e ds
	evalDict =<< findEqual e ds

checkPragmas :: ([[String]], Mdl) -> Either String Mdl
checkPragmas (ps, mdl) = if eqPragmas pragmas0 ps
	then pure mdl
	else Left $ "Pragmas should be\n" ++ unlines (unwords <$> pragmas0)

pragmas0 :: [[String]]
pragmas0 = [["OPTIONS_GHC", "-Wall", "-fno-warn-tabs"]]

eqPragmas :: [[String]] -> [[String]] -> Bool
eqPragmas ps0 ps = ps == ps0

checkModule :: Mdl -> Either String (String, [Decl])
checkModule mdl = do
	when ((moduleImport mdl) /= import0)
		. Left $ "should import " ++ show import0
	e <- maybe (Left "need export variables") Right
		. listToMaybe $ moduleExport mdl
	pure (e, moduleDecls mdl)

import0 :: [String]
import0 = ["Hason"]

checkTypeDecl :: String -> Decl -> Maybe String
checkTypeDecl v0 = \case
	TypeDecl v tp | v == [v0] -> Just tp
	_ -> Nothing

checkTypeDecls :: String -> [Decl] -> Either String ()
checkTypeDecls v0 ds = do
	md <- findTypeDecl v0 ds
	when (md /= "Hason") $ Left "type should be Hason"
	pure ()

findTypeDecl :: String -> [Decl] -> Either String String
findTypeDecl v0 = maybe (Left "type should be Hason") Right
	. listToMaybe . mapMaybe (checkTypeDecl v0)

checkEqual :: String -> Decl -> Maybe Exp
checkEqual v0 = \case
	Equation v e | v == v0 -> Just e
	_ -> Nothing

findEqual :: String -> [Decl] -> Either String Exp
findEqual e = maybe (Left $ "no value of " ++ e) Right
	. listToMaybe . mapMaybe (checkEqual e)

evalDict :: Exp -> Either String Hason
evalDict = \case
	ExpList es -> mapM evalDict1 es
	_ -> Left "not Dict"

evalDict1 :: Exp -> Either String (HasonKey, HasonValue)
evalDict1 = \case
	ExpTuple [k, v] -> (,) <$> evalKey k <*> evalValue v
	_ -> Left "not key-value"

evalKey :: Exp -> Either String HasonKey
evalKey = \case
	Con "KInt" :$ Literal (Integer k) -> Right $ KInt k
	Con "KStr" :$ Literal (String k) -> Right $ KStr k
	_ -> Left "not key"

evalValue :: Exp -> Either String HasonValue
evalValue = \case
	Con "Str" :$ Literal (String v) -> Right $ Str v
	Con "Int" :$ Literal (Integer v) -> Right $ Int v
	Con "Seq" :$ ExpList vs -> Seq <$> mapM evalValue vs
	Con "Dct" :$ d -> Dct <$> evalDict d
	_ -> Left "no such value"
