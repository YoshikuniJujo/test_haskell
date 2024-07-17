{

{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Ex3_2.Lexer where

import Data.Bool

}

%wrapper "monadUserState"

tokens :-

<0,comment>$white+			{ skip }

<0,comment>"{-"				{ mkL LOpenComment }
<0,comment>[^$white]* "-}"		{ mkL LCloseComment }

<0>[^$white]+				{ mkL LToken }
<comment>[^$white]+			{ skip }

{

data LexemeClass = LToken | LOpenComment | LCloseComment deriving (Show, Eq)

mkL :: LexemeClass -> AlexInput -> Int -> Alex Token
mkL c (pos, _, _, str) len = case c of
	LToken -> Alex \s@AlexState {
			alex_ust = AlexUserState { commentDepth = d } } ->
		Right (s, Token (t, pos))
	LOpenComment -> Alex \s@AlexState {
			alex_ust = ust@AlexUserState { commentDepth = d } } ->
		f s {	alex_ust = ust { commentDepth = d + 1 },
			alex_scd = comment }
	LCloseComment -> Alex \s@AlexState {
			alex_ust = ust@AlexUserState { commentDepth = d } } ->
		if d > 0
		then f s {
			alex_ust = ust { commentDepth = d - 1 },
			alex_scd = case () of
				_	| d == 1 -> 0
					| d > 1 -> comment }
		else Right (s, Token (t, pos))
	where
	t = take len str
	Alex f = alexMonadScan

alexEOF :: Alex Token
alexEOF = Alex \s@AlexState {
		alex_ust = ust@AlexUserState { commentDepth = d } } ->
	bool (Left "unterminated `{-'") (Right (s, Eof)) (d == 0)

data Token = Token (String, AlexPosn) | Eof deriving Show

data AlexUserState = AlexUserState { commentDepth :: Int }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState { commentDepth = 0 }

}
