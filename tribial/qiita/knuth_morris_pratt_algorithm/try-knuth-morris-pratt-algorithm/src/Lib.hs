module Lib where

import Data.List

matches :: Eq a => [a] -> [a] -> [Int]
matches ws = map length . filter (endswith ws) . inits

endswith :: Eq a => [a] -> [a] -> Bool
endswith ws xs = ws `elem` tails xs
