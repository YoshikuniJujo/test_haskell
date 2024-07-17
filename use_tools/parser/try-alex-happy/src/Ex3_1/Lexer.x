{

{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Ex3_1.Lexer where

import Data.Bool

}

%wrapper "monadUserState"

tokens :-

$white+				;

"{-"				{ mkL LOpenComment }
"-}"				{ mkL LCloseComment }

[^$white]+			{ mkL LToken }

{

data LexemeClass = LToken | LOpenComment | LCloseComment deriving (Show, Eq)

mkL :: LexemeClass -> AlexInput -> Int -> Alex Token
mkL c (pos, _, _, str) len = case c of
	LToken -> Alex \s@AlexState {
			alex_ust = AlexUserState { commentDepth = d } } ->
		bool (f s) (Right (s, Token (t, pos))) (d == 0)
	LOpenComment -> Alex \s@AlexState {
			alex_ust = ust@AlexUserState { commentDepth = d } } ->
		f s { alex_ust = ust { commentDepth = d + 1 } }
	LCloseComment -> Alex \s@AlexState {
			alex_ust = ust@AlexUserState { commentDepth = d } } ->
		f s { alex_ust = ust { commentDepth = d - 1 } }
	where
	t = take len str
	Alex f = alexMonadScan

alexEOF :: Alex Token
alexEOF = Alex
	\s@AlexState { alex_ust = ust@AlexUserState { commentDepth = d } } ->
		bool (Left "unterminated `{-'") (Right (s, Eof)) (d == 0)

data Token = Token (String, AlexPosn) | Eof deriving Show

data AlexUserState = AlexUserState { commentDepth :: Int }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState { commentDepth = 0 }

}
