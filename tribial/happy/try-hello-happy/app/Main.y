{
module Main (main) where
}

%name calc
%tokentype { Token }
%error { parseError }

%token
	let	{ TokenLet }

%%

Exp	: let		{ Let }

{

data Token = TokenLet
	deriving Show

data Exp = Let

main :: IO ()
main = putStrLn "Slozsoft"

parseError = error . show

}
