{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.List.ToolsYj (elemAll, elemNotAll, findDefault) where

import Data.Maybe
import Data.List ((\\), find)

elemAll :: Eq a => [a] -> [a] -> Bool
elemAll es = null . (es \\)

elemNotAll :: Eq a => [a] -> [a] -> Bool
elemNotAll es = not . elemAll es

findDefault :: a -> (a -> Bool) -> [a] -> a
findDefault d p = fromMaybe d . find p
