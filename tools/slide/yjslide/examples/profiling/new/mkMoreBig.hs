testData :: String
testData = concat $ replicate (10 ^ 7) "1234567890"

main :: IO ()
main = writeFile "moreBig.txt" testData
