{-# LANGUAGE TemplateHaskell #-}

module Parser (lexer, parseExp, parseDec) where

import Control.Applicative
import Language.Haskell.TH

import Lexer

parseExp :: [Token] -> (ExpQ, [Token])
parseExp (Var v : ts) = (varE $ mkName v, ts)
parseExp (Nat n : ts) = (litE $ integerL n, ts)
parseExp (Str s : ts) = (litE $ stringL s, ts)
parseExp (OP : CP : ts) = (conE '(), ts)
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
parseDec (OP : Setq : Var v : ts) = let
	(e, CP : ts') = parseExp ts in
	(:)	<$> valD (varP $ mkName v) (normalB e) []
		<*> parseDec ts'
parseDec [] = return []
parseDec ts = error $ show ts
