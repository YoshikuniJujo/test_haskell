{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryParse.Exponential where

import Control.Applicative
import Data.Bool
import Data.List
import Data.Parse

data Expression = One | Expression :+ Expression deriving Show

type Op = Expression -> Expression -> Expression

pExpr, pTerm :: Parse String Expression
pExpr =	(:+) <$> pTerm <* pick '+' <*> pExpr <|> pTerm
pTerm = pick '(' *> pExpr <* pick ')' <|> One <$ pick '1'

pChar :: Parse String Char
pChar = parse uncons

pick :: Char -> Parse String Char
pick = check . (==)

check :: (Char -> Bool) -> Parse String Char
check p = pChar >>= \c -> bool empty (pure c) (p c)
