{

module Ex3_1.Parser where

import Ex3_1.Lexer

}

%name parser
%error { parseError }
%lexer { lexwrap } { Eof }
%monad { Alex }
%tokentype { Token }

%token

TOKEN		{ Token ($$, _) }

%%

tokens		:	TOKEN tokens_t		{ $1 : $2 }
		|	{- empty -}		{ [] }

tokens_t	:	TOKEN tokens_t		{ $1 : $2 }
		|	{- empty -}		{ [] }

{

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (=<< alexMonadScan)

parseError :: Token -> Alex a
parseError t = alexError $ "parseError: " ++ show t

parse = (`runAlex` parser)

}
