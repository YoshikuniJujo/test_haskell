module Main (main) where

import Data.Time

main :: IO ()
main = do
	now <- getCurrentTime
	writeFile "world.txt" $ "WORLD: " ++ show now
