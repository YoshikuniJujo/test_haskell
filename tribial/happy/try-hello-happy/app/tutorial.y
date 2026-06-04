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

%%

Exp1	: Term		{ Term $1 }

Term	: Factor	{ Factor $1 }

Factor	: int		{ Int $1 }

{

data Token
	= TokenLet
	| TokenIn
	| TokenInt Int	
	deriving Show

data Exp
	= Term Exp
	| Factor Exp
	| Int Int

parseError = error . show

main = putStrLn "Slozsoft"

}
