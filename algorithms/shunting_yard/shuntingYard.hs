{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Data.Char

data Token = Int Double | Op Op | Open | Close deriving (Show, Eq)
data Op = Add | Sub | Mul | Div | Pow deriving (Show, Eq)
data Assoc = L | R deriving (Show, Eq)

force :: Op -> Int
force Add = 1
force Sub = 1
force Mul = 2
force Div = 2
force Pow = 3

assoc :: Op -> Assoc
assoc Pow = R
assoc _ = L

lexer :: String -> [Token]
lexer ('+' : src) = Op Add : lexer src
lexer ('-' : src) = Op Sub : lexer src
lexer ('*' : src) = Op Mul : lexer src
lexer ('/' : src) = Op Div : lexer src
lexer ('^' : src) = Op Pow : lexer src
lexer ('(' : src) = Open : lexer src
lexer (')' : src) = Close : lexer src
lexer (d : src) | isDigit d = Int (read $ d : ds) : lexer src'
	where (ds, src') = span isDigit src
lexer (' ' : src) = lexer src
lexer "" = []
lexer _ = error "Oops!"

shuntingYard :: [Token] -> [Token] -> [Token]
shuntingYard st (t@(Int _) : ts) = t : shuntingYard st ts
shuntingYard st@(t2@(Op o2) : st') (t1@(Op o1) : ts)
	| force o1 < force o2 || assoc o1 == L && force o1 == force o2 =
		t2 : shuntingYard (t1 : st') ts
	| otherwise = shuntingYard (t1 : st) ts
shuntingYard st (t1@(Op _) : ts) = shuntingYard (t1 : st) ts
shuntingYard st (t@Open : ts) = shuntingYard (t : st) ts
shuntingYard st (Close : ts) = case span (/= Open) st of
	(os, Open : st') -> reverse os ++ shuntingYard st' ts
	_ -> error "Oops!"
shuntingYard (Open : _) [] = error "Oops!"
shuntingYard (t : ts) [] = t : shuntingYard ts []
shuntingYard [] [] = []

showTokens :: [Token] -> String
showTokens = unwords . map showToken

showToken :: Token -> String
showToken (Int n) = show n
showToken (Op Add) = "+"
showToken (Op Sub) = "-"
showToken (Op Mul) = "*"
showToken (Op Div) = "/"
showToken (Op Pow) = "^"
showToken Open = "("
showToken Close = ")"

op :: Op -> Double -> Double -> Double
op Add = (+)
op Sub = (-)
op Mul = (*)
op Div = (/)
op Pow = (**)

calc :: [Double] -> [Token] -> Double
calc st (Int n : ts) = calc (n : st) ts
calc (b : a : st) (Op o : ts) = calc (op o a b : st) ts
calc (a : _) [] = a
calc _ _ = error "Oops!"
