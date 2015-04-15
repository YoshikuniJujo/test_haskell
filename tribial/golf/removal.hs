main :: IO ()
main = interact run

chars :: [Char]
chars = ['a' .. 'z']

remove :: [Char] -> String -> String
remove = filter . flip notElem

getRemove :: [Char] -> Int -> Char -> [Char] -> [Char]
getRemove p n r (c : s)
	| r == c = take n p ++ [r] ++ take n s
	| otherwise = getRemove (c : p) n r s

run :: String -> String
run (n : s) = remove (getRemove (reverse chars) (read [n]) (last s) $ cycle chars) s
