main :: IO ()
main = interact $ unlines . map (s . isCircle . words) . tail . lines

s :: Bool -> String
s False = "false"
s True= "true"

circles :: String -> [String]
circles s = s : circles (tail s ++ [head s])

isCircle :: [String] -> Bool
isCircle [s1, s2] = s1 `elem` take (length s2) (circles s2)
