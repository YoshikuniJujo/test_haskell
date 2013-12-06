testData :: String
testData = concat $ replicate (10 ^ 5) "1234567890"

main :: IO ()
main = writeFile "1MFile.txt" testData
