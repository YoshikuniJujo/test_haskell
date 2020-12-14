{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Derivation.Parse (
	Memo, Var, parse, givenWanted, given, wanted, bool, polynomial ) where

import Control.Arrow (second)
import Control.Applicative (empty, many, (<|>))
import Data.Function ((&))
import Data.Maybe (listToMaybe)
import Data.List (uncons, unfoldr)
import Data.Char (isLower, isDigit)
import Data.Parse (Parse(..), (>>!), maybeToParse)

import Data.Derivation.CanDerive (Given, Wanted, mkGiven, mkWanted)
import Data.Derivation.Expression (Exp(..), Number)

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
	polynomial :: Maybe (Exp Var Number, Memo),
	number :: Maybe (Exp Var Number, Memo),
	variable :: Maybe (Var, Memo),
	token :: Maybe (String, Memo) }

memo :: [String] -> Memo
memo ts = m where
	m = Memo gw gv wt ct bl eq le ex nm vr tk
	gw = unparse pGivenWanted m; gv = unparse pGiven m; wt = unparse pWanted m
	ct = unparse pConstraint m; bl = unparse pBool m; eq = unparse pEqual m; le = unparse pLessEqual m
	ex = unparse pPolynomial m; nm = unparse pNumber m; vr = unparse pVariable m
	tk = (memo `second`) <$> uncons ts

check :: (String -> Bool) -> Parse Memo String
check p = Parse token >>= \t -> B.bool empty (pure t) (p t)

pick :: String -> Parse Memo String
pick = check . (==)

---------------------------------------------------------------------------
-- GRAMMAR
---------------------------------------------------------------------------

-- GIVEN WANTED, GIVEN AND WANTED

pGivenWanted :: Parse Memo (Given Var, Wanted Var)
pGivenWanted = (,) <$> Parse given <*> Parse wanted

pGiven :: Parse Memo (Given Var)
pGiven = mkGiven <$> (
	pick "given" *> pick ":" *>
	pick "{" *> many (Parse constraint) <* pick "}" )

pWanted :: Parse Memo (Wanted Var)
pWanted = maybeToParse . mkWanted
	=<< pick "wanted" *> pick ":" *> Parse constraint

-- CONSTRAINT

pConstraint :: Parse Memo (Exp Var Bool)
pConstraint =
	Parse equal <|> Parse lessEqual <|>
	Bool False <$ pick "F" <|> Bool True <$ pick "T"

pBool :: Parse Memo (Exp Var Bool)
pBool =
	pick "(" *> Parse equal <* pick ")" <|> Parse lessEqual <|>
	Bool False <$ pick "F" <|> Bool True <$ pick "T"

pEqual :: Parse Memo (Exp Var Bool)
pEqual =
	(:==) <$> var <* pick "==" <*> var >>! (pick "+" <|> pick "-") <|>
	(:==) <$> var <* pick "==" <*> Parse polynomial <|>
	(:==) <$> var <* pick "==" <*> Parse bool <|>
	(:==) <$> Parse polynomial <* pick "==" <*> Parse polynomial <|>
	(:==) <$> Parse bool <* pick "==" <*> Parse bool
	where var = Var <$> Parse variable

pLessEqual :: Parse Memo (Exp Var Bool)
pLessEqual = (:<=) <$> Parse polynomial <* pick "<=" <*> Parse polynomial

-- POLYNOMIAL

pPolynomial :: Parse Memo (Exp Var Number)
pPolynomial = foldl (&) <$> Parse number <*> many (
	(flip (:+) <$> (pick "+" *> Parse number)) <|>
	(flip (:-) <$> (pick "-" *> Parse number)) )

pNumber :: Parse Memo (Exp Var Number)
pNumber =
	Const . read <$> check (all isDigit) <|> Var <$> Parse variable <|>
	pick "(" *> Parse polynomial <* pick ")"

pVariable :: Parse Memo (Var)
pVariable = check $ all isLower

---------------------------------------------------------------------------
-- VAR
---------------------------------------------------------------------------

type Var = String
