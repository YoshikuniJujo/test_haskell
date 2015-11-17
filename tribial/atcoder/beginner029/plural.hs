main :: IO ()
main = interact $ (++ "s\n") . head . lines
