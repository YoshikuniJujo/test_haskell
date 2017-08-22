main :: IO ()
main = interact $ (++ "\n") . show . length . filter (not . null) . lines
