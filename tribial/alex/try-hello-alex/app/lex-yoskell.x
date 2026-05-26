{

{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

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

data Token'
	= OParen | CParen | OBrace | CBrace
	| ColonColon | Equal
	| VarId' String | ConId' String
	| Integer' Integer
	| String' String
	| TokenError Token
	deriving Show

tokenToToken' :: Token -> Token'
tokenToToken' = \case
	Special "(" -> OParen; Special ")" -> CParen
	Special "{" -> OBrace; Special "}" -> CBrace
	ReservedOp "::" -> ColonColon
	ReservedOp "=" -> Equal
	VarId i -> VarId' i; ConId i -> ConId' i
	Integer i -> Integer' i
	String s -> String' s
	tkn -> TokenError tkn

main :: IO ()
main = do
	s <- getContents
	print $ tokenToToken' <$> alexScanTokens s

}
