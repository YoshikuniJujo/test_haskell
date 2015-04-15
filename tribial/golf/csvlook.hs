main :: IO ()
main = interact $ unlines . format . map readComma . lines

readComma :: String -> [String]
readComma "" = [""]
readComma (',' : s) = "" : readComma s
readComma (c : s) = let h : t = readComma s in (c : h) : t

getLength :: [[[a]]] -> [Int]
getLength [xs] = map length xs
getLength (xs : xss) = zipWith max (map length xs) $ getLength xss

mkLine :: [Int] -> [String] -> String
mkLine ls s = "| " ++
	init (concatMap ((' ' :) . (++ " |")) $ zipWith addN ls s) ++ " |"
	where
	addN n s = s ++ replicate (n - length s) ' '

mkSep :: [Int] -> String
mkSep ls = "|-" ++
	init (concatMap ((++ "+") . (`replicate` '-') . (+ 2)) ls) ++ "-|"

format :: [[String]] -> [String]
format sa@(s : ss) = let ls = getLength sa in
	mkSep ls : mkLine ls s : mkSep ls : map (mkLine ls) ss ++ [mkSep ls]
