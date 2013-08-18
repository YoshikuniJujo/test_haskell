{-# LANGUAGE QuasiQuotes, TypeFamilies #-}

module Parser (
	stoneParse,
	Program,
	Statement(..),
	Block,
	Expr,
	Factor,
	Primary(..),
	Number,
	Identifier,
	Op,

	Function
) where

import Prelude hiding (lex)
import Data.Char
import Data.Maybe
import Text.Papillon

import Control.Applicative

import Operator

type Program = [Statement]
data Statement
	= If Expr Block (Maybe Block)
	| While Expr Block
	| Expr Expr
	deriving Show
type Block = [Statement]
type Expr = Primary
type Factor = Primary
type Function = ([Identifier], Block)
data Primary
	= PNumber Number
	| PIdentifier Identifier
	| PString String
	| PNegative Primary
	| POp Op
	| PInfix Primary Op Primary
	| PFunction Function -- [Identifier] Block
	| PClosure Function -- [Identifier] Block
	| PApply Primary [Primary]
	deriving Show
type Number = Int
type Identifier = String
type Op = String

stoneParse :: String -> Maybe Program
stoneParse src = case runError $ program $ parse src of
	Right (r, _) -> Just r
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
	| Comma
	| EOL
	deriving Show

lex :: String -> [StoneToken]
lex str = case runError $ testLexer $ parse str of
	Right (r, _) -> r
	Left _ -> error "lex error"

getPower :: Primary -> (Int, Inf)
getPower (POp "=") = (-1, R)
getPower (POp "||") = (2, R)
getPower (POp "&&") = (3, R)
getPower (POp op)
	| op `elem` ["==", "<", "<=", ">", ">="] = (4, L)
	| op `elem` ["+", "-"] = (6, L)
	| op `elem` ["*", "/", "%"] = (7, L)
	| op `elem` ["^"] = (8, R)
getPower t = error $ "getPower: " ++ show t ++ " is not operator"

reduce :: Primary -> Primary -> Primary -> Primary
reduce l (POp o) r = PInfix l o r
reduce _ o _ = error $ "reduce: " ++ show o ++ " is not operator"

[papillon|

primary' :: Primary
	= p:primary al:postfix?			{ maybe p (PApply p) al }

primary :: Primary
	= (Identifier "fun"):lexer pl:paramList b:block
						{ PClosure (pl, b) }
	/ OParen:lexer e:expr CParen:lexer	{ e }
	/ (Number n):lexer			{ PNumber n }
	/ (Identifier i):lexer			{ PIdentifier i }
	/ (String s):lexer			{ PString s }

factor :: Factor
	= (Op "-"):lexer p:primary'		{ PNegative p }
	/ p:primary'				{ p }

expr :: Expr
	= hf:factor ofs:((Op op):lexer f:factor { [POp op, f] })*
		{ operator getPower reduce $ hf : concat ofs }

block :: Block
	= OBrace:lexer _:(EOL:lexer)?
		s0:statement
		ss:(_:(EOL:lexer / Semicolon:lexer) s:statement { s })*
		_:(EOL:lexer)?
		CBrace:lexer			{ s0 : ss }

statement :: Statement
	= (Identifier "if"):lexer t:expr ib:block
		eb:((Identifier "else"):lexer b:block { b })?
						{ If t ib eb }
	/ (Identifier "while"):lexer t:expr b:block
						{ While t b }
	/ e:expr al:args?	{ maybe (Expr e) (Expr . PApply e) al }

program :: Program
	= ss:(ds:(d:def { Expr d } / s:statement { s })
		_:(EOL:lexer / Semicolon:lexer) { ds })+
						{ ss }

param :: Identifier
	= (Identifier i):lexer			{ i }

params :: [Identifier] = p0:param ps:(Comma:lexer p:param { p })*
						{ p0 : ps }

paramList :: [Identifier]
	= OParen:lexer ps:params? CParen:lexer	{ fromMaybe [] ps }

def :: Primary
	= (Identifier "def"):lexer (Identifier n):lexer pl:paramList b:block
		{ PInfix (PIdentifier n) "=" (PFunction (pl, b)) }

args :: [Expr] = e0:expr es:(Comma:lexer e:expr { e })*
						{ e0 : es }

postfix :: [Expr] = OParen:lexer as:args? CParen:lexer
						{ fromMaybe [] as }

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
	/ _:spaces ','				{ Comma }
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
