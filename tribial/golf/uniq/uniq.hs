import Data.List

main :: IO ()
main = interact $ unlines . map head . filter ((>1) . length) . group . sort . lines
