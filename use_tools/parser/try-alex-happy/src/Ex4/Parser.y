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
SEMI		{ LSemi }
OTHER_T		{ OtherToken $$ _ }

%%

do	:	DO LBRACE others close 		{ $3 }

others	:	OTHER_T SEMI others		{ $1 : $3 }
	|	OTHER_T				{ [$1] }
	|	{- empty -}			{ [] }

close	:	RBRACE				{ () }
	|	error				{% void popIndent }

{

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (=<< alexMonadScan)

parseError :: Token -> Alex a
parseError t = alexError $ "parseError: " ++ show t

parse = (`runAlex` parser)

}
