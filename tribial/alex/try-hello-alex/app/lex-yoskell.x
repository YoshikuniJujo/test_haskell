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
	$lower $alphaNum*		{ VarIdRaw }
	$upper $alphaNum*		{ ConIdRaw }
	$digit+				{ IntegerRaw . read }
	\" ($graphicS | $space)* \"	{ StringRaw . read }

{

data TokenRaw
	= Special String
	| ReservedOp String
	| VarIdRaw String
	| ConIdRaw String
	| IntegerRaw Integer
	| StringRaw String
	deriving Show

data Token
	= OParen | CParen | OBrace | CBrace
	| ColonColon | Equal
	| VarId String | ConId String
	| Integer Integer
	| String String
	| TokenError TokenRaw
	deriving Show

fromRaw :: TokenRaw -> Token
fromRaw = \case
	Special "(" -> OParen; Special ")" -> CParen
	Special "{" -> OBrace; Special "}" -> CBrace
	ReservedOp "::" -> ColonColon
	ReservedOp "=" -> Equal
	VarIdRaw i -> VarId i; ConIdRaw i -> ConId i
	IntegerRaw i -> Integer i
	StringRaw s -> String s
	tkn -> TokenError tkn

main :: IO ()
main = do
	s <- getContents
	print $ fromRaw <$> alexScanTokens s

}
