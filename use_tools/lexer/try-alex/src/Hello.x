{

{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Hello where

import Data.Maybe
import Data.Bool

import Relex

}

%wrapper "monadUserState"

tokens :-

<0>		$white* "{-#"		{ setRelex pragma alexMonadScan }
<0>		$white+			{ \(_, _, _, src) ln ->
						pure . White $ take ln src }

<pragma>	"#-}"			{ relex pragmaToWhites 0 alexMonadScan }
<pragma>	([.\n] # [\#])+		{ withRelex Pragma }

{

data Token
	= White String | Pragma String
	| Eof
	deriving (Show, Eq)

pragmaToWhites :: String -> String
pragmaToWhites = \case
	"" -> ""
	'\t' : cs -> '\t' : pragmaToWhites cs
	'\n' : cs -> '\n' : pragmaToWhites cs
	_ : cs -> ' ' : pragmaToWhites cs

instance RelexMonad Alex where
	type RelexInput Alex = (AlexPosn, Char, [Byte], String)
	type RelexPosn Alex = AlexPosn

	toRelexPosn (ps, _, _, _) = ps
	fromRelexPosn ps (_, x, y, src) = (ps, x, y, src)
	toRelexSource (_, _, _, src) = src
	fromRelexSource src (ps, x, y, _) = (ps, x, y, src)

	setRelexPosn = alexSetRelexPosn
	getRelexPosn = alexGetRelexPosn
	pushRelexWord = alexPushRelexWord
	readRelexWords = alexReadRelexWords
	clearRelexWords = alexClearRelexWords
	setRelexInput = alexSetInput
	setRelexStartCode = alexSetStartCode

alexEOF :: Alex Token
alexEOF = pure Eof

data AlexUserState = AlexUserState {
	relexPosn :: Maybe AlexPosn,
	relexWords :: [String] }
	deriving Show

alexInitUserState :: AlexUserState
alexInitUserState =
	AlexUserState { relexPosn = Nothing, relexWords = [] }

alexSetRelexPosn :: AlexPosn -> Alex ()
alexSetRelexPosn ps = alexModifyUserState \us -> us { relexPosn = Just ps }

alexGetRelexPosn :: Alex AlexPosn
alexGetRelexPosn = fromJust . relexPosn <$> alexGetUserState

alexPushRelexWord :: String -> Alex ()
alexPushRelexWord w = alexModifyUserState \us@AlexUserState { relexWords = ws } ->
	us { relexWords = w : ws }

alexReadRelexWords :: String -> Alex String
alexReadRelexWords w = do
	ws <- relexWords <$> alexGetUserState
	pure . concat . reverse $ w : ws

alexClearRelexWords :: Alex ()
alexClearRelexWords = alexModifyUserState \un -> un { relexWords = [] }

alexModifyUserState :: (AlexUserState -> AlexUserState) -> Alex ()
alexModifyUserState f = alexSetUserState . f =<< alexGetUserState

tryLex :: String -> Either String [Token]
tryLex = (`runAlex` lexAll)

lexAll :: Alex [Token]
lexAll = alexMonadScan >>= \t -> bool ((t :) <$> lexAll) (pure []) (t == Eof)

}
