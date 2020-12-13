{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryParse.Exponential where

import Control.Applicative
import Data.Bool
import Data.List
import Data.Parse

data Expression = One | Expression :+ Expression | Expression :- Expression
	deriving Show

type Op = Expression -> Expression -> Expression

pOpen, pClose :: Parse String ()
pOpen = () <$ pick '('
pClose = () <$ pick ')'

pAdd, pSub :: Parse String Op
pAdd = (:+) <$ pick '+'
pSub = (:-) <$ pick '-'

pOne :: Parse String Expression
pOne = One <$ pick '1'

pChar :: Parse String Char
pChar = parse uncons

pick :: Char -> Parse String Char
pick = check . (==)

check :: (Char -> Bool) -> Parse String Char
check p = pChar >>= \c -> bool empty (pure c) (p c)
