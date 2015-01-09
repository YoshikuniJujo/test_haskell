{-# LANGUAGE QuasiQuotes #-}

module Parser (
	prsf,
	prs,
	dpt,
	showObj,
	showObjM,
) where

import Text.Papillon
import Data.Char

import Object

prsf :: String -> Maybe [Object]
prsf src = case runError $ scmf $ parse src of
	Right (r, _) -> Just r
	_ -> Nothing

prs :: String -> Maybe Object
prs src = case runError $ scm $ parse src of
	Right (r, _) -> Just r
	_ -> Nothing

dpt :: String -> Maybe Int
dpt src = case runError $ depth $ parse src of
	Right (r, _) -> Just r
	_ -> Nothing

data Tkn
	= TIntL Integer
	| TDoubleL Double
	| TStringL String
	| TVar String
	| TTrue
	| TFalse
	| TOParen
	| TCParen
	| TDot
	| TQuote

isVar :: Char -> Bool
isVar = (||) <$> isAlphaNum <*> (`elem` "+-*/<=>?^")

[papillon|

scmf :: [Object]
	= os:obj* _:spaces !_		{ os }

depth :: Int
	= d:depth_ _:spaces !_		{ d }

depth_ :: Int
	= TOParen:lx _:obj* d:depth	{ d + 1 }
	/ TCParen:lx			{ - 1 }
	/				{ 0 }

scm :: Object
	= o:obj _:spaces !_	{ o }

obj :: Object
	= (TIntL i):lx		{ OInt i }
	/ (TDoubleL d):lx	{ ODouble d }
	/ (TStringL s):lx	{ OString s }
	/ (TVar v):lx		{ OVar v }
	/ TOParen:lx os:obj* TCParen:lx
				{ foldr imcons ONil os }
	/ TOParen:lx as:obj* TDot:lx d:obj TCParen:lx
				{ foldr imcons d as }
	/ TQuote:lx o:obj	{ imcons (OVar "quote") $ imcons o ONil }
	/ TTrue:lx		{ OBool True }
	/ TFalse:lx		{ OBool False }

lx :: Tkn
	= _:spaces w:word	{ w }

word :: Tkn
	= mm:('-' { '-' })? n:<isDigit>+ '.' d:<isDigit>+
				{ TDoubleL $ read $ maybe (n ++ "." ++ d)
					(: n ++ "." ++ d) mm }
	/ mm:('-' { '-' })? ds:<isDigit>+
				{ TIntL $ read $ maybe ds (: ds) mm }
	/ s:string		{ TStringL s }
	/ v:<isVar>+		{ TVar v }
	/ '('			{ TOParen }
	/ ')'			{ TCParen }
	/ '.'			{ TDot }
	/ '\''			{ TQuote }
	/ '#' 't'		{ TTrue }
	/ '#' 'f'		{ TFalse }

string :: String = '"' s:(<(`notElem` "\"\\")> / '\\' c:esc { c })* '"'
				{ s }

esc :: Char
	= 'n'			{ '\n' }

spaces = _:<isSpace>*

|]
