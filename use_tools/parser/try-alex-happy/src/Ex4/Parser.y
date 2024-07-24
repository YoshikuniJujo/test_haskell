{

module Ex4.Parser where

import Control.Monad
import Ex4.Lexer

}

%name parser
%error { parseError }
%lexer { lexwrap } { Eof }
%monad { Alex }
%tokentype { Token }

%token

MODULE		{ Module }
DO		{ Do }
WHERE		{ Where }
'{'		{ LBrace }
'}'		{ RBrace }
VLBRACE		{ VLBrace }
VRBRACE		{ VRBrace }
';'		{ Semi }
VARID		{ Varid $$ }
CONID		{ Conid $$ }
'('		{ LParen }
')'		{ RParen }
','		{ Comma }
'='		{ Equal }

INTEGER		{ IntegerLiteral $$ }
STRING		{ StringLiteral $$ }

%%

module	:	MODULE modid '(' exports ')' WHERE body
       						{ ($2, $4, $7) }

modid	:	CONID				{ $1 }

exports	:					{ [] }
	|	export				{ [$1] }
	|	export ',' exports		{ $1 : $3 }

export	:	VARID				{ $1 }

body	:	'{' topdecls '}'		{ $2 }
	|	VLBRACE topdecls vrbrace	{ $2 }

topdecls:					{ [] }
	|	topdecl ';' topdecls		{ $1 : $3 }

topdecl	:	decl				{ $1 }

decl	:	pat rhs				{ ($1, $2) }

rhs	:	'=' exp				{ $2 }

exp	:	infixexp			{ $1 }

infixexp:	lexp				{ $1 }

lexp	:	fexp				{ $1 }

fexp	:	aexp				{ $1 }

aexp	:	literal				{ $1 }

pat	:	lpat				{ $1 }

lpat	:	apat				{ $1 }

apat	:	VARID				{ $1 }

do	:	DO '{' others '}'		{ $3 }
	|	DO VLBRACE others vrbrace 	{ $3 }

vrbrace	:	VRBRACE				{ () }
	|	error				{% void popIndent }

others	:	other ';' others		{ maybe id (:) $1 $3 }
	|	other				{ maybe id (:) $1 [] }

other	:	VARID				{ Just $1 }
	|	{- empty -}			{ Nothing }

literal	:	INTEGER				{ Integer $1 }
	|	STRING				{ String $1 }

{

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (=<< alexMonadScan)

parseError :: Token -> Alex a
parseError t = alexError $ "parseError: " ++ show t

parse = (`runAlex` parser)

data Literal = Integer Integer | String String deriving Show

}
