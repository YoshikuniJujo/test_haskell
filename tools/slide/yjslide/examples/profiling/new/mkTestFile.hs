testData :: String
testData = concat $ replicate (10 ^ 6) "1234567890"

main :: IO ()
main = writeFile "big.txt" testData
