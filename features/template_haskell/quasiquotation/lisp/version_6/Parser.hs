{-# LANGUAGE TemplateHaskell #-}

module Parser (lexer, parseExp, parseDec, parsePat, parseType) where

import Control.Applicative
import Language.Haskell.TH

import Lexer

parseExp :: [Token] -> (ExpQ, [Token])
parseExp (Con v : ts) = (conE $ mkName v, ts)
parseExp (Var v : ts) = (varE $ mkName v, ts)
parseExp (Nat n : ts) = (litE $ integerL n, ts)
parseExp (Str s : ts) = (litE $ stringL s, ts)
parseExp (OP : CP : ts) = (conE '(), ts)
parseExp (OP : Lambda : OP : ts) = let
	(ps, ts') = parsePatList ts
	(es, ts'') = parseList ts' in
	(lamE ps $ last es, ts'')
parseExp (OP : ts) = let
	(es, ts') = parseList ts in
	(foldl1 appE es, ts')
parseExp ts = error $ "parseExp: parse error: " ++ show ts

parseList :: [Token] -> ([ExpQ], [Token])
parseList (CP : ts) = ([], ts)
parseList ts = let
	(e, ts') = parseExp ts
	(es, ts'') = parseList ts' in
	(e : es, ts'')

parseDec :: [Token] -> DecsQ
parseDec (OP : Define : Var v : ts) = let
	(e, CP : ts') = parseExp ts in
	(:)	<$> valD (varP $ mkName v) (normalB e) []
		<*> parseDec ts'
parseDec (OP : Define : OP : Var v : ts) = let
	(ps, ts') = parsePatList ts
	(es, ts'') = parseList ts' in
	(:)	<$> valD
			(varP $ mkName v)
			(normalB . lamE ps $ last es)
			[]
		<*> parseDec ts''
parseDec [] = return []
parseDec ts = error $ show ts

parsePat :: [Token] -> (PatQ, [Token])
parsePat (Var v : ts) = (varP $ mkName v, ts)
parsePat (OP : Con v : ts) = let
	(ps, ts') = parsePatList ts in
	(conP (mkName v) ps, ts')
parsePat ts =
	error $ "parsePattern: parse error: " ++ show ts

parsePatList :: [Token] -> ([PatQ], [Token])
parsePatList (CP : ts) = ([], ts)
parsePatList ts = let
	(p, ts') = parsePat ts
	(ps, ts'') = parsePatList ts' in
	(p : ps, ts'')

parseType :: [Token] -> (TypeQ, [Token])
parseType (Con v : ts) = (conT $ mkName v, ts)
parseType (OP : ts) = let
	(tps, ts') = parseTypeList ts in
	(foldl1 appT tps, ts')
parseType ts = error $ "parseType: parse error: " ++ show ts

parseTypeList :: [Token] -> ([TypeQ], [Token])
parseTypeList (CP : ts) = ([], ts)
parseTypeList ts = let
	(tp, ts') = parseType ts
	(tps, ts'') = parseTypeList ts' in
	(tp : tps, ts'')
