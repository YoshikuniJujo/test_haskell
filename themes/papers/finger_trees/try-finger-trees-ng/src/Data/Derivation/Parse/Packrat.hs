{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Derivation.Parse.Packrat where

import Control.Applicative hiding (Const)
import Data.Function
import Data.Maybe
import Data.List
import Data.Char

import Data.Parse
import Data.Derivation.Expression

import qualified Data.Bool as B

data Derivs = Derivs {
	bool :: Maybe (Exp String Bool, Derivs),
	lesserEqual :: Maybe (Exp String Bool, Derivs),
	expression :: Maybe (Exp String Term, Derivs),
	number :: Maybe (Exp String Term, Derivs),
	variable :: Maybe (String, Derivs),
	token :: Maybe (String, Derivs) }

parse :: (Derivs -> Maybe (a, Derivs)) -> String -> Maybe a
parse t str = fst <$> t (derivs $ tokens str)

tokens :: String -> [String]
tokens = unfoldr (listToMaybe . lex)

derivs :: [String] -> Derivs
derivs ts = d where
	d = Derivs bl le ex nm vr tk
	bl = pBool d
	le = pLesserEqual d
	ex = pExpression d
	nm = pNumber d
	vr = pVariable d
	tk = case ts of
		t : ts' -> Just (t, derivs ts')
		_ -> Nothing

check :: (String -> Bool) -> Parse Derivs String
check p = do
	t <- Parse token
	B.bool (fail "parse fail") (pure t) (p t)

pick :: String -> Parse Derivs String
pick = check . (==)

pBool :: Derivs -> Maybe (Exp String Bool, Derivs)
Parse pBool =
	(Bool False <$ pick "F") <|>
	(Bool True <$ pick "T") <|>
	Parse lesserEqual -- <|> Parse equal

pLesserEqual :: Derivs -> Maybe (Exp String Bool, Derivs)
Parse pLesserEqual = (:<=) <$> (Parse expression <* pick "<=") <*> Parse expression

pExpression :: Derivs -> Maybe (Exp String Term, Derivs)
Parse pExpression = (\t ts -> foldl (&) t ts) <$> Parse number
	<*> many (
		(flip (:+) <$> (pick "+" *> Parse number)) <|>
		(flip (:-) <$> (pick "-" *> Parse number)) )

pNumber :: Derivs -> Maybe (Exp String Term, Derivs)
Parse pNumber =
	(Const . read <$> check (all isDigit)) <|>
	(Var <$> Parse variable) <|>
	(pick "(" *> Parse expression <* pick ")")

pVariable :: Derivs -> Maybe (String, Derivs)
Parse pVariable = check (all isLower)
