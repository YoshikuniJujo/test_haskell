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
SEMI		{ Semi }
VARID		{ Varid $$ }

%%

do	:	DO LBRACE others RBRACE		{ $3 }
	|	DO VLBRACE others vrbrace 	{ $3 }

vrbrace	:	VRBRACE				{ () }
	|	error				{% void popIndent }

others	:	other SEMI others		{ maybe id (:) $1 $3 }
	|	other				{ maybe id (:) $1 [] }

other	:	VARID				{ Just $1 }
	|	{- empty -}			{ Nothing }

{

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (=<< alexMonadScan)

parseError :: Token -> Alex a
parseError t = alexError $ "parseError: " ++ show t

parse = (`runAlex` parser)

}
