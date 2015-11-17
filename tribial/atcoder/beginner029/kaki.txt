main :: IO ()
main = interact $ (++ "\n") . show . length . filter ('r' `elem`) . lines
