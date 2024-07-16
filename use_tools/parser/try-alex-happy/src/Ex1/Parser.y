{

module Ex1.Parser where

import Ex1.Lexer

}

%name parse
%tokentype	{ Token }
%error		{ parseError }

%token
'('		{ LParen _ }
')'		{ RParen _ }
'+'		{ Plus _ }
'-'		{ Minus _ }
'*'		{ Times _ }
'/'		{ Div _ }
INT		{ Int ($$, _) }

%left '+' '-'
%left '*' '/'
%nonassoc UMINUS

%%
exp
	: INT			{ $1 }
	| '(' exp ')'		{ $2 }
	| '-' exp %prec UMINUS	{ 0 - $2 }
	| exp '+' exp		{ $1 + $3 }
	| exp '-' exp		{ $1 - $3 }
	| exp '*' exp		{ $1 * $3 }
	| exp '/' exp		{ $1 `div` $3 }

{

parseError :: [Token] -> a
parseError [] = error "Parse error at EOF"
parseError (t : _) = error $ "Parse error: " ++ show t

calc :: String -> Integer
calc = parse . alexScanTokens

}
