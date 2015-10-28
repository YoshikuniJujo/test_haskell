import Books

main :: IO ()
main = interact $ fromBooklist . read
