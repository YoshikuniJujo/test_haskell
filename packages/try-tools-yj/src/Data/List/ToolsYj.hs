{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.List.ToolsYj (elemAll, elemNotAll, findDefault, listToTuple4) where

import Data.Maybe
import Data.List ((\\), find)

elemAll :: Eq a => [a] -> [a] -> Bool
elemAll es = null . (es \\)

elemNotAll :: Eq a => [a] -> [a] -> Bool
elemNotAll es = not . elemAll es

findDefault :: a -> (a -> Bool) -> [a] -> a
findDefault d p = fromMaybe d . find p

listToTuple4 :: [a] -> (a, a, a, a)
listToTuple4 [r, g, b, a] = (r, g, b, a)
listToTuple4 _ = error "The length of the list is not 4"
