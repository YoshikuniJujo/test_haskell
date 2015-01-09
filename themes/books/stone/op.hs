import Data.Char

showInfix :: Token -> String
showInfix (Infix l o r) =
	"(" ++ showInfix l ++ " " ++ [o] ++ " " ++ showInfix r ++ ")"
showInfix (Atom d) = [d]

data Token
	= Infix Token Char Token
	| Atom Char
	deriving Show

power :: Char -> Int
power '=' = 0
power '|' = 1
power '&' = 2
power '+' = 3
power '-' = 3
power '*' = 4
power '/' = 4
power '%' = 4
power '^' = 5

op :: [Token] -> String -> Token
op [t] "" = t
op [] (c : cs) = op [Atom c] cs
op [t] (c : cs) = op [Atom c, t] cs
op s@(Atom o : _) (c : cs)
	| not (isDigit o) = op (Atom c : s) cs
op (l : Atom o : r : rest) "" = op (Infix r o l : rest) ""
op s@(l : Atom o : r : rest) ca@(c : cs)
	| power o < power c = op (Atom c : s) cs
	| otherwise = op (Infix r o l : rest) ca
op st src = error $ show st ++ " " ++ show src
