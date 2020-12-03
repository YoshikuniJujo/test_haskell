{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Derivation.Parse where

import Control.Applicative hiding (Const)
import Data.Function
import Data.Maybe
import Data.List
import Data.Char

import Data.Parse
import Data.Derivation.CanDerive
import Data.Derivation.Expression

import qualified Data.Bool as B

type WantedSet v = (Maybe (Wanted v), [Wanted v])

data Derivs = Derivs {
	testData :: Maybe ((Given String, WantedSet String), Derivs),
	given :: Maybe (Given String, Derivs),
	wanted :: Maybe (WantedSet String, Derivs),
	constraint :: Maybe (Exp String Bool, Derivs),
	bool :: Maybe (Exp String Bool, Derivs),
	equal :: Maybe (Exp String Bool, Derivs),
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
	d = Derivs td gv wt ct bl eq le ex nm vr tk
	td = pTestData d
	gv = pGiven d
	wt = pWanted d
	ct = pConstraint d
	bl = pBool d
	eq = pEqual d
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

pTestData :: Derivs -> Maybe ((Given String, WantedSet String), Derivs)
Parse pTestData = (,) <$> Parse pGiven <*> Parse pWanted

pGiven :: Derivs -> Maybe (Given String, Derivs)
Parse pGiven = expsToGiven <$> (pick "given" *> pick ":" *> pick "{" *> many (Parse constraint) <* pick "}")

pWanted :: Derivs -> Maybe (WantedSet String, Derivs)
Parse pWanted = expToWanted <$> (pick "wanted" *> pick ":" *> Parse constraint)

pConstraint :: Derivs -> Maybe (Exp String Bool, Derivs)
Parse pConstraint =
	Parse equal <|> Parse lesserEqual <|>
	(Bool False <$ pick "F") <|> (Bool True <$ pick "T")

pBool :: Derivs -> Maybe (Exp String Bool, Derivs)
Parse pBool =
	Parse lesserEqual <|> (pick "(" *> Parse equal <* pick ")") <|>
	(Bool False <$ pick "F") <|> (Bool True <$ pick "T")

pEqual :: Derivs -> Maybe (Exp String Bool, Derivs)
Parse pEqual =
	((:==) . Var <$> Parse variable <*> (pick "==" *> (Var <$> Parse variable) >>! (pick "+" <|> pick "-"))) <|>
	((:==) . Var <$> Parse variable <*> (pick "==" *> Parse expression)) <|>
	((:==) . Var <$> Parse variable <*> (pick "==" *> Parse bool)) <|>
	((:==) <$> Parse expression <*> (pick "==" *> Parse expression)) <|>
	((:==) <$> Parse bool <*> (pick "==" *> Parse bool))

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
