import Triv
import System.Environment
import Data.Tree
import Control.Arrow
import TreeTools
import Data.List

type Result = Tree (String, [(String, String)])

data WordsTree
	= WNode String [(String, String)] [WordsTree]
	deriving Show

type CountTree = Tree (String, Int)

countWords :: Result -> Int
countWords (Node (_, ws) ts) = length ws + sum (map countWords ts)

resultToCountTree :: Result -> CountTree
resultToCountTree n@(Node (sn, ws) ts) =
--	Node (sn, countWords n) $ map resultToCountTree ts
	Node (sn, length ws) $ map resultToCountTree ts

showCount ind (sn, num) =
	replicate (ind * 4) ' ' ++ sn ++
	replicate (32 - length sn - ind * 4) ' ' ++
	" (" ++ show num ++ ")\n"

printCountTree :: CountTree -> IO ()
printCountTree = putStr . showTreeIndented showCount 0

main = getArgs >>= getParsed . head >>=
	mapM_ (putStr . showTreeIndented showWords 0) . mkWTree
--	mapM_ (putStr . showTreeIndented showSecOnly 0) . mkWTree
--	mapM_ (printCountTree . resultToCountTree) . mkWTree

showSecOnly :: Int -> (String, [(String, String)]) -> String
showSecOnly _ (sec, _) = sec

showWords :: Int -> (String, [(String, String)]) -> String
showWords ind (sec, ws) =
	replicate (ind * 4) ' ' ++ sec ++ "\n" ++
	unlines (map ((replicate (ind * 4 + 6) ' ' ++) . showW) ws)

showW :: (String, String) -> String
showW (jbo, en) = jbo ++ "\t" ++ en

getParsed fn = do
	tex <- readFile fn
	return $ filterWanted $ eval tex

wanted = ["w", "section", "subsection", "subsubsection"]

filterWanted = filter ((`elem` wanted) . fst)

checkSecs :: String -> [(String, [String])] -> [(String, [(String, [String])])]
checkSecs _ [] = []
checkSecs sec ((s, [sn]) : rest)
	| sec == s = (sn, ws) : checkSecs sec ns
	| otherwise = error $ "bad: " ++ show s ++ " - " ++ show (head rest)
	where (ws, ns) = span ((/= sec) . fst) rest

checkSections :: [(String, [String])] -> [(String, [(String, [String])])]
checkSections = checkSecs "section"

type Words = [(String, [String])]
type Words2 = [(String, String)]

wordToWord2 :: (String, [String]) -> (String, String)
wordToWord2 ("w", [jbo, en]) = (jbo, en)

changeBr (a, (b, c)) = ((a, b), c)

checkSubsections :: Words ->
--	(Words2, [(String, (Words2, [(String, Words2)]))])
	(Words2, [((String, Words2), [(String, Words2)])])
checkSubsections src =
	(map wordToWord2 ws :: Words2,
	map (changeBr . second checkSubsubsections) $ checkSecs "subsection" subs)
	where (ws, subs) = span ((/= "subsection") . fst) src

-- checkAll :: Words -> [(String, (Words, [(String, Words)]))]
checkAll = map (changeBr . second checkSubsections) . checkSecs "section"

checkSubsubsections :: Words -> (Words2, [(String, Words2)])
checkSubsubsections src =
	(map wordToWord2 ws,
	map (second $ map wordToWord2) $ checkSecs "subsubsection" subs)
	where (ws, subs) = span ((/= "subsubsection") . fst) src

-- toWTreeSubsub :: [(String, Words2)] -> [WordsTree]
toWTreeSubsub = map $ \(subsub, ws) -> Node (subsub, ws) []

-- toWTreeSub :: [((String, Words2), [(String, Words2)])] -> [WordsTree]
toWTreeSub = map $ \((sub, ws), subsubs) -> Node (sub, ws) $ toWTreeSubsub subsubs

-- toWTree :: [((String, Words2), [((String, Words2), [(String, Words2)])])] ->
--	[WordsTree]
toWTree = map $ \((sec, ws), subs) -> Node (sec, ws) $ toWTreeSub subs

mkWTree = toWTree . checkAll
