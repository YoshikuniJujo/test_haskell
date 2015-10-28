import Books

main :: IO ()
main = interact $ maybe "" ((++ "\n") . show) . booklist
