{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Derivation.Parse (
	Memo, Var, parseIt, givenWanted, given, wanted, bool, polynomial ) where

import Control.Arrow (second)
import Control.Applicative (empty, many, (<|>))
import Data.Function ((&))
import Data.Maybe (listToMaybe)
import Data.List (uncons, unfoldr)
import Data.Char (isLower, isDigit)
import Data.Parse (Parse, parse, unparse, (>>!), maybeToParse)

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

parseIt :: (Memo -> Maybe (a, Memo)) -> String -> Maybe a
parseIt n = (fst <$>) . n . memo . unfoldr (listToMaybe . lex)

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
	token :: Maybe (String, Memo) }

type Var = String

memo :: [String] -> Memo
memo ts = m where
	m = Memo gw gv wt ct bl eq le ex nm tk
	gw = unparse pGivenWanted m; gv = unparse pGiven m; wt = unparse pWanted m
	ct = unparse pConstraint m; bl = unparse pBool m; eq = unparse pEqual m; le = unparse pLessEqual m
	ex = unparse pPolynomial m; nm = unparse pNumber m
	tk = (memo `second`) <$> uncons ts

check :: (String -> Bool) -> Parse Memo String
check p = parse token >>= \t -> B.bool empty (pure t) (p t)

pick :: String -> Parse Memo String
pick = check . (==)

---------------------------------------------------------------------------
-- GRAMMAR
---------------------------------------------------------------------------

-- GIVEN WANTED, GIVEN AND WANTED

pGivenWanted :: Parse Memo (Given Var, Wanted Var)
pGivenWanted = (,) <$> parse given <*> parse wanted

pGiven :: Parse Memo (Given Var)
pGiven = mkGiven <$> (
	pick "given" *> pick ":" *>
	pick "{" *> many (parse constraint) <* pick "}" )

pWanted :: Parse Memo (Wanted Var)
pWanted = maybeToParse . mkWanted
	=<< pick "wanted" *> pick ":" *> parse constraint

-- CONSTRAINT

pConstraint :: Parse Memo (Exp Var Bool)
pConstraint =
	parse equal <|> parse lessEqual <|>
	Bool False <$ pick "F" <|> Bool True <$ pick "T"

pBool :: Parse Memo (Exp Var Bool)
pBool =
	pick "(" *> parse equal <* pick ")" <|> parse lessEqual <|>
	Bool False <$ pick "F" <|> Bool True <$ pick "T"

pEqual :: Parse Memo (Exp Var Bool)
pEqual =
	(:==) <$> var <* pick "==" <*> var >>! (pick "+" <|> pick "-") <|>
	(:==) <$> var <* pick "==" <*> parse polynomial <|>
	(:==) <$> var <* pick "==" <*> parse bool <|>
	(:==) <$> parse polynomial <* pick "==" <*> parse polynomial <|>
	(:==) <$> parse bool <* pick "==" <*> parse bool
	where var = Var <$> check (all isLower)

pLessEqual :: Parse Memo (Exp Var Bool)
pLessEqual = (:<=) <$> parse polynomial <* pick "<=" <*> parse polynomial

-- POLYNOMIAL

pPolynomial :: Parse Memo (Exp Var Number)
pPolynomial = foldl (&) <$> parse number <*> many (
	(flip (:+) <$> (pick "+" *> parse number)) <|>
	(flip (:-) <$> (pick "-" *> parse number)) )

pNumber :: Parse Memo (Exp Var Number)
pNumber =
	Const . read <$> check (all isDigit) <|> Var <$> check (all isLower) <|>
	pick "(" *> parse polynomial <* pick ")"
