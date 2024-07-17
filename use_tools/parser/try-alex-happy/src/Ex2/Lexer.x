{

{-# OPTIONS_GHC -fno-warn-tabs #-}

module Ex2.Lexer where
import qualified Data.Map as Map

}

%wrapper "monadUserState"

$digit = [0-9]
$alpha = [a-zA-Z]

token :-

$white+			;

$digit+			{ mkL LInt }

"("			{ mkL LLParen }
")"			{ mkL LRParen }
";"			{ mkL LSemi }

[\+\-\*\/]+		{ mkL LOp }

"infixl"		{ mkL LInfixl }

$alpha+			{ mkL LName }

{

data LexemeClass = LInt | LName | LOp | LLParen | LRParen | LSemi | LInfixl
	deriving (Show, Eq)

mkL c (pos, _, _, str) len = let t = take len str in
	case c of
		LInt -> pure $ Int (read t, pos)
		LName -> pure $ Name (t, pos)
		LInfixl ->pure $ Infixl pos
		LLParen -> pure $ LParen pos
		LRParen -> pure $ RParen pos
		LSemi -> pure $ Semi pos
		LOp -> Alex $ (\s@AlexState {
			alex_ust = AlexUserState { dict = dict } } ->
				case Map.lookup t dict of
					Just (name, 0) -> Right (s, Op0 (name, pos))
					Just (name, 1) -> Right (s, Op1 (name, pos))
					_ -> Right (s, Name (t, pos)))

alexEOF :: Alex Token
alexEOF = return Eof

data Token
	= Int (Integer, AlexPosn)
	| Name (String, AlexPosn)
	| Op0 (String, AlexPosn) | Op1 (String, AlexPosn)
	| Infixl AlexPosn
	| LParen AlexPosn | RParen AlexPosn | Semi AlexPosn
	| Eof
	deriving Show

data AlexUserState = AlexUserState {
	dict :: Map.Map String (String, Integer) }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState { dict = Map.empty }

}
