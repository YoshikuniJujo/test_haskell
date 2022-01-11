{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Template.ModuleTree where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

data Tree a = Node a [Tree a] deriving Show

moduleTree :: Int -> Module -> Q (Tree Module)
moduleTree d m | d < 1 = pure $ Node m []
moduleTree d m = do
	ModuleInfo ms <- reifyModule m
	Node m <$> moduleTree (d - 1) `mapM` ms
