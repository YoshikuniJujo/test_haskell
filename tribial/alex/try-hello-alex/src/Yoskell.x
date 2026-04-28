{
module Yoskell where
}

%wrapper "basic"

$lower = a-z
$upper = A-Z
$alpha = [$lower $upper]
$digit = [0-9]

tokens :-

	$white+				;
	"--".*				;
	\(				{ \_ -> OpenParen }
	\)				{ \_ -> CloseParen }
	\{				{ \_ -> OpenBrace }
	\}				{ \_ -> CloseBrace }
	\:\:				{ \_ -> ColonColon }
	\=				{ \_ -> Equal }
	$upper [$alpha $digit \_ \']*	{ \s -> UpperVar s }
	$lower [$alpha $digit \_ \']*	{ \s -> Var s }
	$digit+				{ \s -> Int (read s) }


{

data Token
	= OpenParen | CloseParen | OpenBrace | CloseBrace
	| ColonColon | Equal
	| UpperVar String | Var String
	| Int Int
	deriving Show

}
