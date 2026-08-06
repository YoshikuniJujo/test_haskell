module Tools where

fileNameN :: String -> String -> Int -> FilePath
fileNameN bs ex n = bs ++ showInt2 n ++ "." ++ ex

showInt2 :: Int -> String
showInt2 n = replicate (2 - length s) '0' ++ s where s = show n
