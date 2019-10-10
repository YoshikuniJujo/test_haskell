import NanoScheme

main :: IO ()
main = interact $ either show (unlines . map showValue . fst) . (`scheme` env0)
