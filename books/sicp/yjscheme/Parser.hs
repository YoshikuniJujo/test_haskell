{-# LANGUAGE QuasiQuotes #-}

module Parser (
	prs,
	showObj
) where

import Text.Papillon
import Data.Char

import Object

prs :: String -> Maybe Object
prs src = case runError $ scm $ parse src of
	Right (r, _) -> Just r
	_ -> Nothing

data Tkn
	= TIntL Integer
	| TDoubleL Double
	| TVar String
	| TOParen
	| TCParen
	| TDot

isVar :: Char -> Bool
isVar = (||) <$> isAlpha <*> (`elem` "+-*/<=>?")

[papillon|

scm :: Object
	= o:obj _:spaces !_	{ o }

obj :: Object
	= (TIntL i):lx		{ OInt i }
	/ (TDoubleL d):lx	{ ODouble d }
	/ (TVar v):lx		{ OVar v }
	/ TOParen:lx os:obj* TCParen:lx
				{ foldr OCons ONil os }
	/ TOParen:lx as:obj* TDot:lx d:obj TCParen:lx
				{ foldr OCons d as }

lx :: Tkn
	= _:spaces w:word	{ w }

word :: Tkn
	= n:<isDigit>+ '.' d:<isDigit>+
				{ TDoubleL $ read $ n ++ "." ++ d }
	/ ds:<isDigit>+		{ TIntL $ read ds }
	/ v:<isVar>+		{ TVar v }
	/ '('			{ TOParen }
	/ ')'			{ TCParen }
	/ '.'			{ TDot }

spaces = _:<isSpace>*

|]
