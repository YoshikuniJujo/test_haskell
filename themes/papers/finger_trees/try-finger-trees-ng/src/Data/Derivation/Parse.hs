{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Derivation.Parse (
	Memo, Var, parse, givenWanted, given, wanted, bool, expression ) where

import Control.Arrow (second)
import Control.Applicative (empty, many, (<|>))
import Data.Function ((&))
import Data.Maybe (listToMaybe)
import Data.List (uncons, unfoldr)
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
--	+ GIVEN WANTED, GIVEN AND WANTED
--	+ CONSTRAINT
--	+ POLYNOMIAL
-- * VAR
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
	givenWanted :: Maybe ((Given Var, Wanted Var), Memo),
	given :: Maybe (Given Var, Memo),
	wanted :: Maybe (Wanted Var, Memo),
	constraint :: Maybe (Exp Var Bool, Memo),
	bool :: Maybe (Exp Var Bool, Memo),
	equal :: Maybe (Exp Var Bool, Memo),
	lessEqual :: Maybe (Exp Var Bool, Memo),
	expression :: Maybe (Exp Var Term, Memo),
	number :: Maybe (Exp Var Term, Memo),
	variable :: Maybe (Var, Memo),
	token :: Maybe (String, Memo) }

memo :: [String] -> Memo
memo ts = m where
	m = Memo gw gv wt ct bl eq le ex nm vr tk
	gw = pGivenWanted m; gv = pGiven m; wt = pWanted m
	ct = pConstraint m; bl = pBool m; eq = pEqual m; le = pLessEqual m
	ex = pExpression m; nm = pNumber m; vr = pVariable m
	tk = (memo `second`) <$> uncons ts

check :: (String -> Bool) -> Parse Memo String
check p = Parse token >>= \t -> B.bool empty (pure t) (p t)

pick :: String -> Parse Memo String
pick = check . (==)

---------------------------------------------------------------------------
-- GRAMMAR
---------------------------------------------------------------------------

-- GIVEN WANTED, GIVEN AND WANTED

pGivenWanted :: Memo -> Maybe ((Given Var, Wanted Var), Memo)
Parse pGivenWanted = (,) <$> Parse pGiven <*> Parse pWanted

pGiven :: Memo -> Maybe (Given Var, Memo)
Parse pGiven = expsToGiven <$> (
	pick "given" *> pick ":" *>
	pick "{" *> many (Parse constraint) <* pick "}" )

pWanted :: Memo -> Maybe (Wanted Var, Memo)
Parse pWanted = maybeToParse . expToWanted
	=<< pick "wanted" *> pick ":" *> Parse constraint

-- CONSTRAINT

pConstraint :: Memo -> Maybe (Exp Var Bool, Memo)
Parse pConstraint =
	Parse equal <|> Parse lessEqual <|>
	Bool False <$ pick "F" <|> Bool True <$ pick "T"

pBool :: Memo -> Maybe (Exp Var Bool, Memo)
Parse pBool =
	pick "(" *> Parse equal <* pick ")" <|> Parse lessEqual <|>
	Bool False <$ pick "F" <|> Bool True <$ pick "T"

pEqual :: Memo -> Maybe (Exp Var Bool, Memo)
Parse pEqual =
	(:==) <$> var <* pick "==" <*> var >>! (pick "+" <|> pick "-") <|>
	(:==) <$> var <* pick "==" <*> Parse expression <|>
	(:==) <$> var <* pick "==" <*> Parse bool <|>
	(:==) <$> Parse expression <* pick "==" <*> Parse expression <|>
	(:==) <$> Parse bool <* pick "==" <*> Parse bool
	where var = Var <$> Parse variable

pLessEqual :: Memo -> Maybe (Exp Var Bool, Memo)
Parse pLessEqual = (:<=) <$> Parse expression <* pick "<=" <*> Parse expression

-- POLYNOMIAL

pExpression :: Memo -> Maybe (Exp Var Term, Memo)
Parse pExpression = (\t ts -> foldl (&) t ts) <$> Parse number
	<*> many (
		(flip (:+) <$> (pick "+" *> Parse number)) <|>
		(flip (:-) <$> (pick "-" *> Parse number)) )

pNumber :: Memo -> Maybe (Exp Var Term, Memo)
Parse pNumber =
	(Const . read <$> check (all isDigit)) <|>
	(Var <$> Parse variable) <|>
	(pick "(" *> Parse expression <* pick ")")

pVariable :: Memo -> Maybe (Var, Memo)
Parse pVariable = check (all isLower)

---------------------------------------------------------------------------
-- VAR
---------------------------------------------------------------------------

type Var = String
