{
module Main where
}

%name calc
%tokentype { Token }
%error { parseError }

%token
	let		{ TokenLet }
	in		{ TokenIn }
	int		{ TokenInt $$ }
	var		{ TokenVar $$ }
	'='		{ TokenEq }
	'+'		{ TokenPlus }
	'-'		{ TokenMinus }
	'*'		{ TokenTimes }
	'/'		{ TokenDiv }
	'('		{ TokenOB }
	')'		{ TokenCB }

%%

Exp1	: Term		{ Term $1 }

Term	: Factor	{ Factor $1 }

Factor	: int		{ Int $1 }

{

data Token
	= TokenLet
	| TokenIn
	| TokenInt Int	
	| TokenVar String
	| TokenEq
	| TokenPlus
	| TokenMinus
	| TokenTimes
	| TokenDiv
	| TokenOB
	| TokenCB
	deriving Show

data Exp
	= Term Exp
	| Factor Exp
	| Int Int

parseError = error . show

main = putStrLn "Slozsoft"

}
