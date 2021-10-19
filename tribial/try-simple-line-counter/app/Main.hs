module Main where

main :: IO ()
main = interact $ (++ "\n") . show . countLine

countLine :: String -> Int
countLine = length . filter (not . null) . lines
