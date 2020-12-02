{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Internal.Tools.SampleParser where

import Control.Applicative
import Data.Function
import Data.Char

import Internal.Tools.Parse

number :: Parse Integer
number = read <$> some (check isDigit)

spaces1 :: Parse ()
spaces1 = () <$ some (check isSpace)

spaces :: Parse ()
spaces = () <$ many (check isSpace)

comma :: Parse ()
comma = () <$ (spaces >*> char ',' >*> spaces)

sep :: Parse ()
sep = spaces1 <|> comma

numbers :: Parse [Integer]
numbers = uncurry (:) <$> (number >*> many (sep *> number))

type Op = Integer -> Integer -> Integer

op, adsb, mldv, ad, sb, ml, dv :: Parse Op
op = ad <|> sb <|> ml <|> dv
adsb = ad <|> sb
mldv = ml <|> dv
ad = (+) <$ char '+'
sb = (-) <$ char '-'
ml = (*) <$ char '*'
dv = div <$ char '/'

expr :: Parse Integer
expr = (\x o y -> x `o` y) <$> term <*> op <*> term

term :: Parse Integer
term = number <|> (char '(' *> expr <* char ')')

calc :: String -> Maybe Integer
calc = parse expr

expr' :: Parse Integer
expr' = (\x oys -> foldl (&) x oys) <$> term' <*> many adsbExpr'

adsbExpr' :: Parse (Integer -> Integer)
adsbExpr' = (\o y -> (`o` y)) <$> adsb <*> term'

term' :: Parse Integer
term' =	((\x oys -> foldl (&) x oys) <$> number <*> many mldvTerm') <|>
	(char '(' *> expr' <* char ')')

mldvTerm' :: Parse (Integer -> Integer)
mldvTerm' = (\o y -> (`o` y)) <$> mldv <*> number

calc' :: String -> Maybe Integer
calc' = parse expr'

foo', bar' :: Integer
foo' = foldr ($) 2 [(+ 5), (* 2), (+ 3), (* 4)] -- (((2 * 4) + 3) * 2) + 5
bar' = foldl (&) 2 [(+ 5), (* 2), (+ 3), (* 4)] -- (((2 + 5) * 2) + 3) * 4
