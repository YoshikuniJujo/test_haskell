module Automaton where

data Door = White | Black deriving Show

ctd :: Char -> Door
ctd '*' = Black
ctd '.' = White
