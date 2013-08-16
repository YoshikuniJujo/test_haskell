{-# LANGUAGE QuasiQuotes, TypeFamilies #-}

module Parser (
) where

import Prelude hiding (lex)
import Data.Char
import Text.Papillon

import Control.Applicative

import Infix

power :: Op -> Int
power "*" = 7
power "%" = 7
power "/" = 7
power "+" = 6
power "-" = 6
power "=" = 0
power "<" = 3
power "==" = 3
power other = error other

ix :: Statement -> Statement
ix (If e b mb) = If (ixe e) (map ix b) (map ix <$> mb)
ix (While e b) = While (ixe e) (map ix b)
ix (Expr e) = Expr $ ixe e

ixe :: Expr -> Expr
ixe = mapInfix ixf . solveInfix power

ixp :: Primary -> Primary
ixp (PExpr e) = PExpr $ ixe e
ixp o = o

ixf :: Factor -> Factor
ixf (Positive p) = Positive $ ixp p
ixf (Negative p) = Negative $ ixp p

type Program = [Statement]
data Statement
	= If Expr Block (Maybe Block)
	| While Expr Block
	| Expr Expr
	deriving Show
type Block = [Statement]
type Expr = Infix Op Factor
data Factor = Positive Primary | Negative Primary deriving Show
data Primary
	= PExpr Expr
	| PNumber Number
	| PIdentifier Identifier
	| PString String
	deriving Show
type Number = Int
type Identifier = String
type Op = String

stoneParse :: String -> Maybe Program
stoneParse src = case runError $ program $ parse src of
	Right (r, _) -> Just $ map ix r
	Left _ -> Nothing

data StoneToken
	= Identifier String
	| Op String
	| Number Int
	| String String
	| OParen
	| CParen
	| OBrace
	| CBrace
	| Semicolon
	| EOL
	deriving Show

lex :: String -> [StoneToken]
lex str = case runError $ testLexer $ parse str of
	Right (r, _) -> r
	Left _ -> error "lex error"

[papillon|

primary :: Primary
	= OParen:lexer e:expr CParen:lexer	{ PExpr e }
	/ (Number n):lexer			{ PNumber n }
	/ (Identifier i):lexer			{ PIdentifier i }
	/ (String s):lexer			{ PString s }

factor :: Factor
	= (Op "-"):lexer p:primary		{ Negative p }
	/ p:primary				{ Positive p }

expr :: Expr
	= hf:factor ofs:((Op op):lexer f:factor { (op, Atom f) })*
						{ UInfix (Atom hf) ofs }

block :: Block
	= OBrace:lexer _:(EOL:lexer)?
		ss:(s:statement _:(EOL:lexer / Semicolon:lexer) { s })+
		CBrace:lexer			{ ss }

statement :: Statement
	= (Identifier "if"):lexer t:expr ib:block
		eb:((Identifier "else"):lexer b:block { b })?
						{ If t ib eb }
	/ (Identifier "while"):lexer t:expr b:block
						{ While t b }
	/ e:expr				{ Expr e }

program :: Program
	= ss:(s:statement _:(EOL:lexer / Semicolon:lexer) { s })+
						{ ss }

testLexer :: [StoneToken] = ts:lexer* _:spaces !_:[True]	{ ts }

lexer :: StoneToken
	= _:spaces ds:<isDigit>+		{ Number $ read ds }
	/ _:spaces '"' s:('\\' e:escapeChar { e } / <(/= '"')>)* '"'
						{ String s }
	/ _:spaces h:<(||) <$> isAlpha <*> (`elem` "_")>
		t:<(||) <$> isAlphaNum <*> (`elem` "_")>*
						{ Identifier $ h : t }
	/ _:spaces '('				{ OParen }
	/ _:spaces ')'				{ CParen }
	/ _:spaces '{'				{ OBrace }
	/ _:spaces '}'				{ CBrace }
	/ _:spaces ';'				{ Semicolon }
	/ _:spaces '=' '='			{ Op "==" }
	/ _:spaces '<' '='			{ Op "<=" }
	/ _:spaces '>' '='			{ Op ">=" }
	/ _:spaces '&' '&'			{ Op "&&" }
	/ _:spaces '|' '|'			{ Op "||" }
	/ _:spaces p:<(||) <$> isPunctuation <*> isSymbol>
						{ Op [p] }
	/ _:spaces '\n'				{ EOL }

escapeChar :: Char
	= '"'					{ '"' }
	/ '\\'					{ '\\' }
	/ 'n'					{ '\n' }

spaces = _:<(`elem` " \t")>*

|]
