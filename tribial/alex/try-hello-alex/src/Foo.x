{
module Foo where
}

%wrapper "basic"

tokens :-
	$white+			;
	let			{ \s -> Let }

{

data Token = Let
	deriving (Eq, Show)

}
