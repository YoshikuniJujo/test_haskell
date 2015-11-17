main :: IO ()
main = interact $ unlines . sequence . (`replicate` "abc") . read
