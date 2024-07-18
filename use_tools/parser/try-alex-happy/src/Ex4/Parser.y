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

DO		{ Do }
LBRACE		{ LBrace }
RBRACE		{ RBrace }
VLBRACE		{ VLBrace }
VRBRACE		{ VRBrace }
SEMI		{ LSemi }
OTHER_T		{ OtherToken $$ _ }

%%

do	:	DO LBRACE others RBRACE		{ $3 }
	|	DO VLBRACE others close 	{ $3 }

others	:	other SEMI others		{ $1 ++ $3 }
	|	{- empty -}			{ [] }

other	:	OTHER_T				{ [$1] }
	|	{- empty -}			{ [] }

close	:	VRBRACE				{ () }
	|	error				{% void popIndent }

{

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (=<< alexMonadScan)

parseError :: Token -> Alex a
parseError t = alexError $ "parseError: " ++ show t

parse = (`runAlex` parser)

}
