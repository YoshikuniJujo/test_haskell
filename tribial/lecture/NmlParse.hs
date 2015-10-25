module NmlParse (nml) where

import Data.List (unfoldr)
import Data.Tree (Tree(..))
import Data.Char (isSpace)

type Nml = Tree String

nml :: String -> Maybe Nml
nml s = case parse $ tokens s of (Just n, []) -> Just n; _ -> Nothing

parse :: [Token] -> (Maybe Nml, [Token])
parse (Text tx : ts) = (Just $ Node tx [], ts)
parse (Open tg : s) = case parses s of
	(ns, Close tg' : r) | tg == tg' -> (Just $ Node tg ns, r)
	(ns, r) -> (Just $ Node tg ns, r)
parse ts = (Nothing, ts)

parses :: [Token] -> ([Nml], [Token])
parses ts = case parse ts of
	(Just n, r) -> let (ns, r') = parses r in (n : ns, r')
	(_, r) -> ([], r)

data Token = Open String | Close String | Text String deriving Show

tokens :: String -> [Token]
tokens = concatMap emptyTag . spaces . catTexts . unfoldr token

emptyTag :: Token -> [Token]
emptyTag (Open tg) | last tg == '/' = map ($ init tg) [Open, Close]
emptyTag tg = [tg]

spaces :: [Token] -> [Token]
spaces (Text tx : ts) | all isSpace tx = spaces ts
spaces (Text tx : ts) = Text tx' : spaces ts
	where tx' = takeWhileR (not . isSpace) $ dropWhile isSpace tx
spaces (t : ts) = t : spaces ts
spaces _ = []

takeWhileR :: (a -> Bool) -> [a] -> [a]
takeWhileR = twr [] where
	twr s p (x : xs)
		| p x = reverse s ++ [x] ++ twr [] p xs
		| otherwise = twr (x : s) p xs
	twr _ _ _ = []

catTexts :: [Token] -> [Token]
catTexts (Text tx1 : Text tx2 : ts) = catTexts $ Text (tx1 ++ tx2) : ts
catTexts (t : ts) = t : catTexts ts
catTexts _ = []

token :: String -> Maybe (Token, String)
token "" = Nothing
token ('<' : s) = Just $ tag s
token ('&' : s) = entity s
token s = Just $ let (tx, r) = span (`notElem` "<&") s in (Text tx, r)

tag :: String -> (Token, String)
tag ('/' : s) = case span (/= '>') s of
	(tg, _ : r) -> (Close tg, r)
	(tg, _) -> (Close tg, "")
tag s = case span (/= '>') s of
	(tg, _ : r) -> (Open tg, r)
	(tg, _) -> (Open tg, "")

entity :: String -> Maybe (Token, String)
entity ('l' : 't' : ';' : s) = Just (Text "<", s)
entity ('g' : 't' : ';' : s) = Just (Text ">", s)
entity ('a' : 'm' : 'p' : ';' : s) = Just (Text "&", s)
entity _ = Nothing
