main :: IO ()
main = interact ((++ "\n") . show . sum . map read . lines)
