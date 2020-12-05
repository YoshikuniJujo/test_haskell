{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Derivation.Parse (
	Derivs, parse, givenWanted, given, wanted, bool, expression ) where

import Control.Applicative (empty, many, (<|>))
import Data.Function ((&))
import Data.Maybe (listToMaybe)
import Data.List (unfoldr)
import Data.Char (isLower, isDigit)
import Data.Parse (Parse(..), (>>!))

import Data.Derivation.CanDerive (Given, WantedSet, expsToGiven, expToWanted)
import Data.Derivation.Expression (Exp(..), Term)

import qualified Data.Bool as B (bool)

---------------------------------------------------------------------------
--
-- * PARSE
-- * DERIVS
-- * GRAMMAR
--	+ TEST DATA, GIVEN AND WANTED
--	+ CONSTRAINT
--	+ POLYNOMIAL
--
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- PARSE
---------------------------------------------------------------------------

parse :: (Derivs -> Maybe (a, Derivs)) -> String -> Maybe a
parse n = (fst <$>) . n . derivs . unfoldr (listToMaybe . lex)

---------------------------------------------------------------------------
-- DERIVS
---------------------------------------------------------------------------

data Derivs = Derivs {
	givenWanted :: Maybe ((Given String, WantedSet String), Derivs),
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

derivs :: [String] -> Derivs
derivs ts = d where
	d = Derivs td gv wt ct bl eq le ex nm vr tk
	td = pGivenWanted d
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
check p = Parse token >>= \t -> B.bool empty (pure t) (p t)

pick :: String -> Parse Derivs String
pick = check . (==)

---------------------------------------------------------------------------
-- GRAMMAR
---------------------------------------------------------------------------

-- TEST DATA, GIVEN AND WANTED

pGivenWanted :: Derivs -> Maybe ((Given String, WantedSet String), Derivs)
Parse pGivenWanted = (,) <$> Parse pGiven <*> Parse pWanted

pGiven :: Derivs -> Maybe (Given String, Derivs)
Parse pGiven = expsToGiven <$> (pick "given" *> pick ":" *> pick "{" *> many (Parse constraint) <* pick "}")

pWanted :: Derivs -> Maybe (WantedSet String, Derivs)
Parse pWanted = expToWanted <$> (pick "wanted" *> pick ":" *> Parse constraint)

-- CONSTRAINT

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

-- POLYNOMIAL

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
