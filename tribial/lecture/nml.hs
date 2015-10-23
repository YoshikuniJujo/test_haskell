import Data.List (unfoldr)
import Data.Tree (Tree(..))
import Data.Char (isSpace)

type Nml = Tree String

nml :: String -> Maybe Nml
nml s = case parse $ tokens s of (Just n, []) -> Just n; _ -> Nothing

parse :: [Token] -> (Maybe Nml, [Token])
parse (Right tx : ts) = (Just $ Node tx [], ts)
parse (Left (Open, tg) : s) = case parses s of
	(ns, Left (Close, tg') : r) | tg == tg' -> (Just $ Node tg ns, r)
	(ns, r) -> (Just $ Node tg ns, r)
parse ts = (Nothing, ts)

parses :: [Token] -> ([Nml], [Token])
parses ts = case parse ts of
	(Just n, r) -> let (ns, r') = parses r in (n : ns, r')
	(_, r) -> ([], r)

type Token = Either (OC, String) String

tokens :: String -> [Token]
tokens = concatMap emptyTag . spaces . catTexts . unfoldr token

emptyTag :: Token -> [Token]
emptyTag (Left (Open, tg)) | last tg == '/' =
	let tg' = init tg in [Left (Open, tg'), Left (Close, tg')]
emptyTag tg = [tg]

spaces :: [Token] -> [Token]
spaces (Right tx : ts) | all isSpace tx = spaces ts
spaces (Right tx : ts) = Right tx' : spaces ts
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
catTexts (Right tx1 : Right tx2 : ts) = catTexts $ Right (tx1 ++ tx2) : ts
catTexts (t : ts) = t : catTexts ts
catTexts _ = []

token :: String -> Maybe (Token, String)
token "" = Nothing
token ('<' : s) = Just $ let (tg, r) = tag s in (Left tg, r)
token ('&' : s) = case entity s of
	Just (e, r) -> Just (Right [e], r)
	_ -> Nothing
token s = Just $ let (tx, r) = span (`notElem` "<&") s in (Right tx, r)

data OC = Open | Close deriving Show

tag :: String -> ((OC, String), String)
tag ('/' : s) = case span (/= '>') s of
	(tg, _ : r) -> ((Close, tg), r)
	(tg, _) -> ((Close, tg), "")
tag s = case span (/= '>') s of
	(tg, _ : r) -> ((Open, tg), r)
	(tg, _) -> ((Open, tg), "")

entity :: String -> Maybe (Char, String)
entity ('l' : 't' : ';' : s) = Just ('<', s)
entity ('g' : 't' : ';' : s) = Just ('>', s)
entity ('a' : 'm' : 'p' : ';' : s) = Just ('&', s)
entity _ = Nothing
