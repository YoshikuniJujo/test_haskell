{

{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Ex4.Lexer where

import Control.Monad
import Data.Bool
import Data.Maybe

import Relex

}

%wrapper "monadUserState"

$small = [a-z]
$large = [A-Z]
$digit = [0-9]
$pragma = ~$white # \#

token :-

<0>			$white* "{-#"	{ setRelex pragma alexMonadScan }
<pragma>		"#-}"		{ relex pragmaToWhites 0 alexMonadScan }
<pragma>		([.\n] # [\#])+	{ withRelex PragmaContent }

<0>			$white+		{ spaces0 }

<0>			$small [$small $large $digit]*
					{ varid }
<0>			$large [$small $large $digit]*
					{ conid }

<0, maybeLayout> 	$white* "{"	{ (`andBegin` 0) \_ _ ->
						LBrace <$ pushIndent 0 }

<maybeLayout>		$white+		{ spacesLayout }
<emptyLayout>		$white+		{ \inp _ -> VRBrace <$ do
						alexSetInput inp
						alexSetStartCode 0 }

<0>			"		{ begin stringLiteral }
<stringLiteral>		(~["])*		{ \(_, _, _, src) len -> pure
						. StringLiteral $ take len src }
<stringLiteral>		"		{ begin 0 }

<0>			$digit+		{ \(_, _, _, src) ln -> pure . IntegerLiteral
						. read $ take ln src }

<0>			"}"		{ \_ _ -> RBrace <$ popIndent_ }
<0>			";"		{ \_ _ -> pure Semi }
<0>			"("		{ \_ _ -> pure LParen }
<0>			")"		{ \_ _ -> pure RParen }
<0>			","		{ \_ _ -> pure Comma }
<0>			"["		{ \_ _ -> pure LBracket }
<0>			"]"		{ \_ _ -> pure RBracket }
<0>			"::"		{ \_ _ -> pure ColonColon }
<0>			"="		{ \_ _ -> pure Equal }

{

data Token
	= PragmaContent String
	| Varid String | Conid String
	| StringLiteral String | IntegerLiteral Integer
	| LBrace | RBrace | VLBrace | VRBrace | Semi
	| LParen | RParen | Comma | LBracket | RBracket
	| Let | In | Where | Do | Of
	| ColonColon | Equal
	| Eof
	deriving (Show, Eq)

pragmaContent :: AlexInput -> Int -> Alex Token
pragmaContent (_, _, _, cs) ln = pure . PragmaContent $ take ln cs

varid :: AlexInput -> Int -> Alex Token
varid (p, _, _, cs) ln =
	t <$ when (t `elem` [Let, Where, Do, Of]) (alexSetStartCode maybeLayout)
	where
	t = fromMaybe <$> Varid <*> (`lookup` keywords) $ take ln cs

conid :: AlexInput -> Int -> Alex Token
conid (p, _, _, cs) ln = pure . Conid $ take ln cs

keywords :: [(String, Token)]
keywords = [("let", Let), ("in", In), ("where", Where), ("do", Do), ("of", Of)]

spaces0 :: AlexInput -> Int -> Alex Token
spaces0 inp@(AlexPn _ _ cl, _, _, cs) ln =
	bool (skip inp ln) (offsetAngleN inp ln cl') nl
	where (nl, cl') = calcColumn False cl (take ln cs)

offsetAngleN :: AlexInput -> Int -> Int -> Alex Token
offsetAngleN inp ln n = peekIndent >>= \case
	Just m	| m == n -> pure Semi
		| n < m -> VRBrace <$ (alexSetInput inp >> popIndent_)
	_ -> skip inp ln

spacesLayout :: AlexInput -> Int -> Alex Token
spacesLayout inp@(AlexPn _ _ cl, _, _, cs) ln =
	offsetBraceN inp . snd $ calcColumn False cl (take ln cs)

offsetBraceN :: AlexInput -> Int -> Alex Token
offsetBraceN inp n = peekIndent >>= \case
	Just m	| n <= m ->
		VLBrace <$ (alexSetInput inp >> alexSetStartCode emptyLayout)
	_ -> VLBrace <$ (pushIndent n >> alexSetStartCode 0)

calcColumn :: Bool -> Int -> String -> (Bool, Int)
calcColumn nl c "" = (nl, c)
calcColumn nl c (' ' : ss) = calcColumn nl (c + 1) ss
calcColumn nl c ('\t' : ss) = calcColumn nl (((c - 1) `div` 8 + 1) * 8 + 1) ss
calcColumn _ _ ('\n' : ss) = calcColumn True 1 ss
calcColumn _ _ _ = error "bad space"

alexEOF :: Alex Token
alexEOF = pure Eof

data AlexUserState = AlexUserState {
	indents :: [Int],
	relexPosn :: Maybe AlexPosn,
	relexWords :: [String] }
	deriving Show

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState {
	indents = [],
	relexPosn = Nothing,
	relexWords = [] }

peekIndent :: Alex (Maybe Int)
peekIndent = listToMaybe . indents <$> alexGetUserState

popIndent :: Alex (Maybe Int)
popIndent = alexGetUserState >>= \case
	AlexUserState { indents = [] } -> pure Nothing
	us@AlexUserState { indents = n : is' } ->
		Just n <$ alexSetUserState us { indents = is' }

popIndent_ :: Alex ()
popIndent_ = void popIndent

pushIndent :: Int -> Alex ()
pushIndent n = alexGetUserState >>= \us@AlexUserState { indents = is } ->
	alexSetUserState us { indents = n : is }

tryLex :: String -> Either String [Token]
tryLex = (`runAlex` lexAll)

lexAll :: Alex [Token]
lexAll = alexMonadScan >>= \t -> bool ((t :) <$> lexAll) (pure []) (t == Eof)

instance RelexMonad Alex where
	type RelexPosn Alex = AlexPosn
	type RelexInput Alex = AlexInput

	toRelexPosn (ps, _, _, _) = ps
	fromRelexPosn ps (_, x, y, src) = (ps, x, y, src)
	toRelexSource (_, _, _, src) = src
	fromRelexSource src (ps, x, y, _) = (ps, x, y, src)

	setRelexPosn ps = alexModifyUserState \us -> us { relexPosn = Just ps }
	getRelexPosn = fromJust . relexPosn <$> alexGetUserState
	pushRelexWord w =
		alexModifyUserState \us@AlexUserState { relexWords = ws } ->
		us { relexWords = w : ws }
	readRelexWords w =
		concat . reverse . (w :) . relexWords <$> alexGetUserState
	clearRelexWords = alexModifyUserState \un -> un { relexWords = [] }

	setRelexInput = alexSetInput
	setRelexStartCode = alexSetStartCode

alexModifyUserState :: (AlexUserState -> AlexUserState) -> Alex ()
alexModifyUserState f = alexSetUserState . f =<< alexGetUserState

pragmaToWhites :: String -> String
pragmaToWhites = \case
	"" -> ""
	'\t' : cs -> '\t' : pragmaToWhites cs
	'\n' : cs -> '\n' : pragmaToWhites cs
	_ : cs -> ' ' : pragmaToWhites cs

}
