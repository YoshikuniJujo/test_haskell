{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Internal.Tools.SampleParser where

import Control.Applicative
import Data.Function
import Data.Char

import Internal.Tools.Parse

number :: Parse Char Integer
number = read <$> some (check isDigit)

spaces1 :: Parse Char ()
spaces1 = () <$ some (check isSpace)

spaces :: Parse Char ()
spaces = () <$ many (check isSpace)

comma :: Parse Char ()
comma = () <$ (spaces >*> token ',' >*> spaces)

sep :: Parse Char ()
sep = spaces1 <|> comma

numbers :: Parse Char [Integer]
numbers = uncurry (:) <$> (number >*> many (sep *> number))

type Op = Integer -> Integer -> Integer

op, adsb, mldv, ad, sb, ml, dv :: Parse Char Op
op = ad <|> sb <|> ml <|> dv
adsb = ad <|> sb
mldv = ml <|> dv
ad = (+) <$ token '+'
sb = (-) <$ token '-'
ml = (*) <$ token '*'
dv = div <$ token '/'

expr :: Parse Char Integer
expr = (\x o y -> x `o` y) <$> term <*> op <*> term

term :: Parse Char Integer
term = number <|> (token '(' *> expr <* token ')')

calc :: String -> Maybe Integer
calc = parse expr

expr' :: Parse Char Integer
expr' = (\x oys -> foldl (&) x oys) <$> term' <*> many adsbExpr'

adsbExpr' :: Parse Char (Integer -> Integer)
adsbExpr' = (\o y -> (`o` y)) <$> adsb <*> term'

term' :: Parse Char Integer
term' =	((\x oys -> foldl (&) x oys) <$> number <*> many mldvTerm') <|>
	(token '(' *> expr' <* token ')')

mldvTerm' :: Parse Char (Integer -> Integer)
mldvTerm' = (\o y -> (`o` y)) <$> mldv <*> number

calc' :: String -> Maybe Integer
calc' = parse expr'

foo', bar' :: Integer
foo' = foldr ($) 2 [(+ 5), (* 2), (+ 3), (* 4)] -- (((2 * 4) + 3) * 2) + 5
bar' = foldl (&) 2 [(+ 5), (* 2), (+ 3), (* 4)] -- (((2 + 5) * 2) + 3) * 4
