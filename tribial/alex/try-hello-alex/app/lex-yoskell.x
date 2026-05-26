{
module Main (main) where
}

%wrapper "basic"

$space = \ 
$digit = 0-9
$lower = a-z
$upper = A-Z
$alpha = [$lower $upper]
$alphaNum = [$alpha $digit]
$ascSymbol = [\!\#]
$special = [\(\)\,\;\[\]\`\{\}]
$graphicS = [$alphaNum $ascSymbol $special \:\']

tokens :-
	$white+				;
	$special			{ Special }
	\:\: | \=			{ ReservedOp }
	$lower $alphaNum*		{ VarId }
	$upper $alphaNum*		{ ConId }
	$digit+				{ Integer . read }
	\" ($graphicS | $space)* \"	{ String . read }

{

data Token
	= Special String
	| ReservedOp String
	| VarId String
	| ConId String
	| Integer Integer
	| String String
	deriving Show

main :: IO ()
main = do
	s <- getContents
	print $ alexScanTokens s

}
