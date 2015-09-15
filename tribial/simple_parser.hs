import Data.Char

type Parse a = String -> [(a, String)]

type Op = Integer -> Integer -> Integer

-- none :: Parse a
-- none _ = []

-- basic functions

succeed :: a -> Parse a
succeed v i = [(v, i)]

check :: (Char -> Bool) -> Parse Char
check p (c : cs) | p c = [(c, cs)]
check _ _ = []

char :: Char -> Parse Char
char = check . (==)

alt :: Parse a -> Parse a -> Parse a
(p1 `alt` p2) i = p1 i ++ p2 i

build :: Parse a -> (a -> b) -> Parse b
build p f i = [ (f x, r) | (x, r) <- p i ]

(>*>) :: Parse a -> Parse b -> Parse (a, b)
(p1 >*> p2) i = [ ((x, y), r') | (x, r) <- p1 i, (y, r') <- p2 r ]

(>*) :: Parse a -> Parse b -> Parse a
p1 >* p2 = (p1 >*> p2) `build` fst

(*>) :: Parse a -> Parse b -> Parse b
p1 *> p2 = (p1 >*> p2) `build` snd

eof :: Parse ()
eof "" = [((), "")]
eof _ = []

-- list

list, list1 :: Parse a -> Parse [a]
list p = succeed [] `alt` list1 p
list1 p = (p >*> list p) `build` uncurry (:)

-- numbers

number :: Parse Integer
number = list1 (check isDigit) `build` read

spaces1 :: Parse ()
spaces1 = list1 (check isSpace) `build` const ()

numbers :: Parse [Integer]
numbers = (number >*> list (spaces1 *> number)) `build` uncurry (:)

-- calculater

op, ad, sb, ml, dv :: Parse Op
op = ad `alt` sb `alt` ml `alt` dv
ad = char '+' `build` const (+)
sb = char '-' `build` const (-)
ml = char '*' `build` const (*)
dv = char '/' `build` const div

calc :: String -> Maybe Integer
calc s = case expr >* eof $ s of
	(r, _) : _ -> Just r
	_ -> Nothing

expr :: Parse Integer
expr = (term >*> op >*> term) `build` \((x, o), y) -> x `o` y

term :: Parse Integer
term = number `alt` (char '(' *> expr >* char ')')
