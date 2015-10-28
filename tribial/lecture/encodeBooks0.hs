import Books0

main :: IO ()
main = interact $ (++ "\n") . fromBooklist . read
