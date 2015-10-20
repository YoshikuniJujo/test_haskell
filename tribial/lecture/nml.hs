import Data.List
import Data.Tree

type Nml = Tree String

nml :: String -> Maybe Nml
nml s = case parse $ tokens s of
	(Just n, []) -> Just n
	_ -> Nothing

parse :: [Token] -> (Maybe Nml, [Token])
parse (Right tx : s) = (Just $ Node tx [], s)
parse (Left (Open, t) : s) = case parses s of
	(ns, Left (Close, t') : r) | t == t' -> (Just $ Node t ns, r)
	(ns, r) -> (Just $ Node t ns, r)
parse s = (Nothing, s)

parses :: [Token] -> ([Nml], [Token])
parses ts = case parse ts of
	(Just n, r) -> let (ns, r') = parses r in (n : ns, r')
	(_, r) -> ([], r)

type Token = Either (Tag, String) String

tokens :: String -> [Token]
tokens = catText . unfoldr token

catText :: [Token] -> [Token]
catText (Right tx1 : Right tx2 : ts) = catText $ Right (tx1 ++ tx2) : ts
catText (t : ts) = t : catText ts
catText _ = []

token :: String -> Maybe (Token, String)
token "" = Nothing
token ('<' : s) = Just $ let (t, r) = tag s in (Left t, r)
token ('&' : s) = case entity s of
	Just (e, r) -> Just (Right [e], r)
	_ -> Nothing
token s = Just $ let (t, r) = span (`notElem` "<&") s in (Right t, r)

data Tag = Open | Close deriving Show

tag :: String -> ((Tag, String), String)
tag ('/' : n) = let (t, _ : r) = span (/= '>') n in ((Close, t), r)
tag n = let (t, _ : r) = span (/= '>') n in ((Open, t), r)

entity :: String -> Maybe (Char, String)
entity ('l' : 't' : ';' : r) = Just ('<', r)
entity ('g' : 't' : ';' : r) = Just ('>', r)
entity ('a' : 'm' : 'p' : ';' : r) = Just ('&', r)
entity _ = Nothing
