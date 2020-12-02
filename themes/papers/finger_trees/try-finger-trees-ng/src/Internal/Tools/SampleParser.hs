{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Internal.Tools.SampleParser where

import Control.Applicative
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

op, ad, sb, ml, dv :: Parse Op
op = ad <|> sb <|> ml <|> dv
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
