{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

main :: IO ()
main = putStr (compose [showAppend n | n <- [0 .. 4]] "")

showAppend :: Int -> ShowS
showAppend n =
	showChar '\n' .
	showString "appendTree" . shows n . showString " :: " .
		showFunType
			(	[fingertree n] ++
				replicate n (tyarg n) ++ [fingertree n] )
			(fingertree n) .
		showString "\n" .
	appendTreeClause n "EmptyT" "xs" (showCons (args n) (showString "xs")) .
	appendTreeClause n "xs" "EmptyT" (showSnoc (showString "xs") (args n)) .
	appendTreeClause n "(Single x)" "xs"
		(showCons ('x' : args n) (showString "xs")) .
	appendTreeClause n "xs" "(Single x)"
		(showSnoc (showString "xs") (args n ++ "x")) .
	appendTreeClause n "(Deep s1 pr1 m1 sf1)" "(Deep s2 pr2 m2 sf2)"
		(showString "Deep (s1" .
			compose [showString " + size " . showChar v | v <- args n] .
			showString " + s2) pr1 (addDigits" . shows n .
			showString " m1 sf1" . showArgList (args n) .
			showString " pr2 m2) sf2") .
	showChar '\n' .
	showString "addDigits" . shows n . showString " :: " .
		showFunType
			([fingertree_node n, digit n] ++ replicate n (tyarg n) ++ [digit n, fingertree_node n])
			(fingertree_node n) .
		showString "\n" .
	compose [addDigitClause n n1 n2 | n1 <- [1 .. 4], n2 <- [1 .. 4]]

fingertree :: Int -> String
fingertree n = tyapp "FingerTree" (tyarg n)

digit n = tyapp "Digit" (tyarg n)

fingertree_node n = tyapp "FingerTree" (tyapp "Node" (tyarg n))

showFunType :: [String] -> String -> String -> String
showFunType ts tr =
	compose [showString t . showString " -> " | t <- ts] . showString tr

tyapp :: String -> String -> String
tyapp tc t = tc ++ " (" ++ t ++ ")"

tyarg :: Int -> String
tyarg n	| n == 0 = "BS.ByteString"
	| otherwise = "Node a"

appendTreeClause ::
	Int -> String -> String -> (String -> String) -> String -> String
appendTreeClause n t1 t2 rhs =
	showString "appendTree" . shows n .
		showChar ' ' . showString t1 . showArgList (args n) .
		showChar ' ' . showString t2 .
		showString " =\n     " . rhs . showChar '\n'

addDigitClause n n1 n2 =
	showString "addDigits" . shows n .
	showString " m1 (" . showDigit vs1 . showChar ')' .
	showArgList vsm .
	showString " (" . showDigit vs2 . showString ") m2" .
	showString " =\n    " .
	showString "appendTree" . shows (length ns) .
	showString " m1" .
	compose [showString " (" . showNode node . showChar ')' | node <- ns] .
	showString " m2" . showChar '\n'
	where
	vs = args (n1 + n + n2)
	vs1 = take n1 vs
	vsm = take n (drop n1 vs)
	vs2 = drop (n1 + n) vs
	ns = nodes vs

data Node a = Node2 a a | Node3 a a a

nodes :: [a] -> [Node a]
nodes [a, b] = [Node2 a b]
nodes [a, b, c] = [Node3 a b c]
nodes [a, b, c, d] = [Node2 a b, Node2 c d]
nodes (a : b : c : xs) = Node3 a b c : nodes xs
nodes _ = error "bad"

showNode :: Node Char -> String -> String
showNode = \case
	(Node2 a b) ->
		showString "node2 " . showChar a . showChar ' ' . showChar b
	(Node3 a b c) ->
		showString "node3 " .
			showChar a . showChar ' ' .
			showChar b . showChar ' ' . showChar c

showDigit :: [Char] -> ShowS
showDigit vs =
	showString (["One", "Two", "Three", "Four"] !! (length vs - 1)) .
	showArgList vs

showArgList :: [Char] -> ShowS
showArgList vs = compose [showChar ' ' . showChar c | c <- vs]

args :: Int -> [Char]
args n = take n ['a' ..]

showCons :: [Char] -> (a -> String) -> a -> String
showCons xs sf = compose [showChar x . showString " `consTree` " | x <- xs] . sf

showSnoc :: (String -> c) -> [Char] -> String -> c
showSnoc sf xs = sf . compose [showString " `snocTree` " . showChar x | x <- xs]

compose :: [a -> a] -> a -> a
compose = flip (foldr id)
