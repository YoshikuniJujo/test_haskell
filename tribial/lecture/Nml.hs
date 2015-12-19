module Nml (Nml, nml, fromNml) where

import Data.List (unfoldr)
import Data.Tree (Tree(..))
import Data.Char (isSpace)

type Nml = Tree String

-- DECODE

nml :: String -> Maybe Nml
nml = maybe Nothing (Just . fst) . parse . unfoldr token

parse :: [Token] -> Maybe (Nml, [Token])
parse (Open tg : ts) = case parseL ts of
	(ns, Close tg' : r) | tg == tg' -> Just (Node tg ns, r)
	_ -> Nothing
parse (Text tx : ts) = Just (Node tx [], ts)
parse _ = Nothing

parseL :: [Token] -> ([Nml], [Token])
parseL ts = flip (maybe ([], ts)) (parse ts) $ \(n, r) ->
	let (ns, r') = parseL r in (n : ns, r')

data Token = Open String | Close String | Text String deriving Show

token :: String -> Maybe (Token, String)
token "" = Nothing
token ('<' : '/' : s) = tag Close s
token ('<' : s) = tag Open s
token s	| all isSpace tx = token r
	| otherwise = Just (Text tx, r)
	where (tx, r) = span (/= '<') s

tag :: (String -> Token) -> String -> Maybe (Token, String)
tag f s = case span (/= '>') s of
	(tg, _ : r) -> Just (f tg, r)
	_ -> Nothing

-- ENCODE

fromNml :: Nml -> String
fromNml = toString 0 . toTokens

idt :: Int -> String -> String
idt i = (replicate i '\t' ++)

opn, cls :: String -> String
opn = ('<' :) . (++ ">")
cls = ("</" ++) . (++ ">")

toString :: Int -> [Token] -> String
toString i (Open tg : Text tx : Close tg' : ts) =
	idt i $ opn tg ++ tx ++ cls tg' ++ "\n" ++ toString i ts
toString i (Open tg : ts) = idt i $ opn tg ++ "\n" ++ toString (i + 1) ts
toString i (Close tg : ts) = idt (i - 1) $ cls tg ++ "\n" ++ toString (i - 1) ts
toString i (Text tx : ts) = idt i $ tx ++ "\n" ++ toString i ts
toString _ _ = ""

toTokens :: Nml -> [Token]
toTokens (Node tx []) = [Text tx]
toTokens (Node tg ns) = Open tg : concatMap toTokens ns ++ [Close tg]

-- SAMPLES

sample1 :: Nml
sample1 = Node "hello" [Node "world" []]
