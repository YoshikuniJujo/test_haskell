{

{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Ex4.Lexer where

import Control.Monad
import Data.Bool
import Data.Maybe

}

%wrapper "monadUserState"

token :-

<0>			$white+		{ \p l -> spaces0 p l }

			[a-z]+		{ mkToken }

<0, maybeLayout> 	"{"		{ \p t -> LBrace <$
						(pushIndent 0 >> begin 0 p t) }

<maybeLayout>		$white+		{ \p l -> spacesLayout p l }

{

data Token
	= LBrace | RBrace | LSemi
	| Let | In | Where | Do | Of
	| OtherToken String AlexPosn
	| Eof
	deriving (Show, Eq)

isLayoutBeginer :: Token -> Bool
isLayoutBeginer = (`elem` [Let, Where, Do, Of])

mkToken :: AlexInput -> Int -> Alex Token
mkToken (pos, _, _, str) len = do
	let	t = getKeyword keywordTable pos (take len str)
	t <$ when (isLayoutBeginer t) (alexSetStartCode maybeLayout)

getKeyword :: [(String, Token)] -> AlexPosn -> String -> Token
getKeyword tbl psn idnt = fromMaybe (OtherToken idnt psn) $ lookup idnt tbl

keywordTable :: [(String, Token)]
keywordTable = [
	("let", Let), ("in", In), ("where", Where), ("do", Do), ("of", Of)
	]

spaces0 :: AlexInput -> Int -> Alex Token
spaces0 inp@(AlexPn _ _ c, _, _, str) len =
--	setOffsetAngleN $ calcColumn c (take len str)
	processOffsetAngleN inp len $ calcColumn c (take len str)

spacesLayout :: AlexInput -> Int -> Alex Token
spacesLayout inp@(AlexPn _ _ c, _, _, str) len =
--	setOffsetBraceN $ calcColumn c (take len str)
	processOffsetBraceN inp $ calcColumn c (take len str)

calcColumn :: Int -> String -> Int
calcColumn c "" = c
calcColumn c (' ' : ss) = calcColumn (c + 1) ss
calcColumn c ('\t' : ss) = calcColumn (((c - 1) `div` 8 + 1) * 8 + 1) ss
calcColumn _ ('\n' : ss) = calcColumn 1 ss
calcColumn _ _ = error "bad space"

alexEOF :: Alex Token
alexEOF = pure Eof

data AlexUserState = AlexUserState {
	offsetAngleN :: Maybe Int,
--	offsetBraceN :: Maybe Int,
	indents :: [Int] }
	deriving Show

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState {
	offsetAngleN = Nothing,
--	offsetBraceN = Nothing,
	indents = [] }

getOffsetAngleN :: Alex (Maybe Int)
getOffsetAngleN = offsetAngleN <$> alexGetUserState

setOffsetAngleN :: Int -> Alex ()
setOffsetAngleN n = do
	ust <- alexGetUserState
	alexSetUserState ust { offsetAngleN = Just n }

processOffsetBraceN :: AlexInput -> Int -> Alex Token
processOffsetBraceN inp n = do
	mm <- peekIndent
	case mm of
		Nothing -> LBrace <$ do
			pushIndent n
			alexSetStartCode 0
		Just m	| n > m -> LBrace <$ do
				pushIndent n
				alexSetStartCode 0
			| otherwise -> LBrace <$ do
				alexSetInput inp
				alexSetStartCode 0

processOffsetAngleN :: AlexInput -> Int -> Int -> Alex Token
processOffsetAngleN inp len n = do
	mm <- peekIndent
	case mm of
		Just m	| m == n -> pure LSemi
			| n < m -> RBrace <$ do
				alexSetInput inp
				popIndent
				pure ()
		_ -> skip inp len

{-
getOffsetBraceN :: Alex (Maybe Int)
getOffsetBraceN = offsetBraceN <$> alexGetUserState

setOffsetBraceN :: Int -> Alex ()
setOffsetBraceN n = do
	ust <- alexGetUserState
	alexSetUserState ust { offsetBraceN = Just n }
-}

pushIndent :: Int -> Alex ()
pushIndent n = do
	ust@AlexUserState { indents = is } <- alexGetUserState
	alexSetUserState ust { indents = n : is }

popIndent :: Alex (Maybe Int)
popIndent = do
	ust@AlexUserState { indents = is } <- alexGetUserState
	case is of
		[] -> pure Nothing
		n : is -> Just n <$ alexSetUserState ust { indents = is }

peekIndent :: Alex (Maybe Int)
peekIndent = do
	AlexUserState { indents = is } <- alexGetUserState
	pure case is of
		[] -> Nothing
		n : _ -> Just n

tryLex :: String -> Either String [Token]
tryLex = (`runAlex` lexAll)

lexAll :: Alex [Token]
lexAll = do
	t <- alexMonadScan
	bool ((t :) <$> lexAll) (pure []) (t == Eof)

}
