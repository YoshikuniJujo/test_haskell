{

{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Ex4.Lexer where

import Control.Monad
import Data.Bool
import Data.Maybe

}

%wrapper "monadUserState"

$small = [a-z]
$large = [A-Z]
$digit = [0-9]

token :-

<0>			$white+		{ spaces0 }

<0>			$small [$small $large $digit]*
					{ varid }

<0, maybeLayout> 	$white* "{"	{ (`andBegin` 0) \_ _ ->
						LBrace <$ pushIndent 0 }

<maybeLayout>		$white+		{ spacesLayout }
<emptyLayout>		$white+		{ \inp _ -> VRBrace <$ do
						alexSetInput inp
						alexSetStartCode 0 }

<0>			"}"		{ \_ _ -> RBrace <$ popIndent_ }
<0>			";"		{ \_ _ -> pure Semi }

{

data Token
	= LBrace | RBrace | VLBrace | VRBrace | Semi
	| Let | In | Where | Do | Of
	| Varid String AlexPosn
	| Eof
	deriving (Show, Eq)

varid :: AlexInput -> Int -> Alex Token
varid (p, _, _, cs) ln =
	t <$ when (t `elem` [Let, Where, Do, Of]) (alexSetStartCode maybeLayout)
	where
	t = fromMaybe <$> (`Varid` p) <*> (`lookup` keywords) $ take ln cs

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

data AlexUserState = AlexUserState { indents :: [Int] } deriving Show

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState { indents = [] }

peekIndent :: Alex (Maybe Int)
peekIndent = listToMaybe . indents <$> alexGetUserState

popIndent :: Alex (Maybe Int)
popIndent = indents <$> alexGetUserState >>= \case
	[] -> pure Nothing
	n : is' -> Just n <$ alexSetUserState AlexUserState { indents = is' }

popIndent_ :: Alex ()
popIndent_ = void popIndent

pushIndent :: Int -> Alex ()
pushIndent n = indents <$> alexGetUserState >>= \is ->
	alexSetUserState AlexUserState { indents = n : is }

tryLex :: String -> Either String [Token]
tryLex = (`runAlex` lexAll)

lexAll :: Alex [Token]
lexAll = alexMonadScan >>= \t -> bool ((t :) <$> lexAll) (pure []) (t == Eof)

}
