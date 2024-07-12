module Main (main) where

import Text.Parsec

main :: IO ()
main = print $ parse parens "" "()(())"

parenSet :: Parsec String () Char
parenSet = char '(' >> many parenSet >> char ')'

parens :: Parsec String () ()
parens = (many parenSet >> eof) <|> eof
