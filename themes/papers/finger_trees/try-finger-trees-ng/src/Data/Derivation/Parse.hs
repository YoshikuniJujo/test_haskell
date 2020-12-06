{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Derivation.Parse (
	Memo, parse, givenWanted, given, wanted, bool, expression ) where

import Control.Applicative (empty, many, (<|>))
import Data.Function ((&))
import Data.Maybe (listToMaybe)
import Data.List (unfoldr)
import Data.Char (isLower, isDigit)
import Data.Parse (Parse(..), (>>!), maybeToParse)

import Data.Derivation.CanDerive (Given, Wanted, expsToGiven, expToWanted)
import Data.Derivation.Expression (Exp(..), Term)

import qualified Data.Bool as B (bool)

---------------------------------------------------------------------------
--
-- * PARSE
-- * MEMO
-- * GRAMMAR
--	+ TEST DATA, GIVEN AND WANTED
--	+ CONSTRAINT
--	+ POLYNOMIAL
--
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- PARSE
---------------------------------------------------------------------------

parse :: (Memo -> Maybe (a, Memo)) -> String -> Maybe a
parse n = (fst <$>) . n . memo . unfoldr (listToMaybe . lex)

---------------------------------------------------------------------------
-- MEMO
---------------------------------------------------------------------------

data Memo = Memo {
	givenWanted :: Maybe ((Given String, Wanted String), Memo),
	given :: Maybe (Given String, Memo),
	wanted :: Maybe (Wanted String, Memo),
	constraint :: Maybe (Exp String Bool, Memo),
	bool :: Maybe (Exp String Bool, Memo),
	equal :: Maybe (Exp String Bool, Memo),
	lesserEqual :: Maybe (Exp String Bool, Memo),
	expression :: Maybe (Exp String Term, Memo),
	number :: Maybe (Exp String Term, Memo),
	variable :: Maybe (String, Memo),
	token :: Maybe (String, Memo) }

memo :: [String] -> Memo
memo ts = m where
	m = Memo td gv wt ct bl eq le ex nm vr tk
	td = pGivenWanted m
	gv = pGiven m
	wt = pWanted m
	ct = pConstraint m
	bl = pBool m
	eq = pEqual m
	le = pLesserEqual m
	ex = pExpression m
	nm = pNumber m
	vr = pVariable m
	tk = case ts of
		t : ts' -> Just (t, memo ts')
		_ -> Nothing

check :: (String -> Bool) -> Parse Memo String
check p = Parse token >>= \t -> B.bool empty (pure t) (p t)

pick :: String -> Parse Memo String
pick = check . (==)

---------------------------------------------------------------------------
-- GRAMMAR
---------------------------------------------------------------------------

-- TEST DATA, GIVEN AND WANTED

pGivenWanted :: Memo -> Maybe ((Given String, Wanted String), Memo)
Parse pGivenWanted = (,) <$> Parse pGiven <*> Parse pWanted

pGiven :: Memo -> Maybe (Given String, Memo)
Parse pGiven = expsToGiven <$> (pick "given" *> pick ":" *> pick "{" *> many (Parse constraint) <* pick "}")

pWanted :: Memo -> Maybe (Wanted String, Memo)
Parse pWanted = maybeToParse . expToWanted =<< (pick "wanted" *> pick ":" *> Parse constraint)

-- CONSTRAINT

pConstraint :: Memo -> Maybe (Exp String Bool, Memo)
Parse pConstraint =
	Parse equal <|> Parse lesserEqual <|>
	(Bool False <$ pick "F") <|> (Bool True <$ pick "T")

pBool :: Memo -> Maybe (Exp String Bool, Memo)
Parse pBool =
	Parse lesserEqual <|> (pick "(" *> Parse equal <* pick ")") <|>
	(Bool False <$ pick "F") <|> (Bool True <$ pick "T")

pEqual :: Memo -> Maybe (Exp String Bool, Memo)
Parse pEqual =
	((:==) . Var <$> Parse variable <*> (pick "==" *> (Var <$> Parse variable) >>! (pick "+" <|> pick "-"))) <|>
	((:==) . Var <$> Parse variable <*> (pick "==" *> Parse expression)) <|>
	((:==) . Var <$> Parse variable <*> (pick "==" *> Parse bool)) <|>
	((:==) <$> Parse expression <*> (pick "==" *> Parse expression)) <|>
	((:==) <$> Parse bool <*> (pick "==" *> Parse bool))

pLesserEqual :: Memo -> Maybe (Exp String Bool, Memo)
Parse pLesserEqual = (:<=) <$> (Parse expression <* pick "<=") <*> Parse expression

-- POLYNOMIAL

pExpression :: Memo -> Maybe (Exp String Term, Memo)
Parse pExpression = (\t ts -> foldl (&) t ts) <$> Parse number
	<*> many (
		(flip (:+) <$> (pick "+" *> Parse number)) <|>
		(flip (:-) <$> (pick "-" *> Parse number)) )

pNumber :: Memo -> Maybe (Exp String Term, Memo)
Parse pNumber =
	(Const . read <$> check (all isDigit)) <|>
	(Var <$> Parse variable) <|>
	(pick "(" *> Parse expression <* pick ")")

pVariable :: Memo -> Maybe (String, Memo)
Parse pVariable = check (all isLower)
