{

module Ex2.Parser where

import Ex2.Lexer
import qualified Data.Map as Map

}

%name parser
%error { parseError }
%lexer { lexwrap } { Eof }
%monad { Alex }
%tokentype { Token }

%token
INT		{ Int ($$, _) }
NAME		{ Name ($$, _) }
OP0		{ Op0 ($$, _) }
OP1		{ Op1 ($$, _) }
INFIXL		{ Infixl _ }
'('		{ LParen _ }
')'		{ RParen _ }
';'		{ Semi _ }

%left OP0
%left OP1
%nonassoc FUN

%%

program :: { Integer }
program:	decs exp		{ $2 }

decs	:	dec ';' decs		{ () }
	|	{- empty -}		{ () }

dec:		INFIXL INT NAME NAME	{% Alex (\s -> regInfixl s $2 $3 $4) }

exp :: { Integer }
exp:		INT			{ $1 }
   |		'(' exp ')'		{ $2 }
   |		exp OP0 exp		{ binop $2 $1 $3 }
   |		exp OP1 exp		{ binop $2 $1 $3 }
   |		NAME exp exp %prec FUN	{ binop $1 $2 $3 }

{

binop :: String -> Integer -> Integer -> Integer
binop op lhs rhs = case op of
	"plus" -> lhs + rhs
	"minus" -> lhs - rhs
	"times" -> lhs * rhs
	"divide" -> lhs `div` rhs

regInfixl :: AlexState ->
	Integer -> String -> String -> Either String (AlexState, ())
regInfixl (s@AlexState { alex_ust = AlexUserState { dict = d } }) prc op f
	| f `notElem` ["plus", "minus", "times", "divide"] =
		Left $ "undefined function: " ++ f
	| prc `notElem` [0, 1] = Left $ "invalid precedence: " ++ show prc
	| otherwise = Right (s { alex_ust = AlexUserState { dict = d' } },  ())
	where d' = Map.insert op (f, prc) d

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (=<< alexMonadScan)

parseError :: Token -> Alex a
parseError t = alexError $ "parseError: " ++ show t

parse = (`runAlex` parser)

}
