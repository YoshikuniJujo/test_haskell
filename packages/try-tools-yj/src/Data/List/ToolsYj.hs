{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.List.ToolsYj (elemAll, elemNotAll) where

import Data.List ((\\))

elemAll :: Eq a => [a] -> [a] -> Bool
elemAll es = null . (es \\)

elemNotAll :: Eq a => [a] -> [a] -> Bool
elemNotAll es = not . elemAll es