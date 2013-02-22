module TreeTools(showTreeIndented, printTreeIndented) where

import Data.Tree
import Data.List

testTree = Node "section" [
	Node "subsection1" [Node "subsubsection" []],
	Node "subsection2" [Node "subsubsection2-1" []]
 ]

showTreeIndented :: (Int -> a -> String) -> Int -> Tree a -> String
showTreeIndented sh ind (Node v s) =
	replicate ind ' ' ++  sh ind v ++ "\n" ++
	concatMap (showTreeIndented sh $ ind + 4) s

printTreeIndented :: Show a => Tree a -> IO ()
printTreeIndented = putStr . showTreeIndented (const show) 0
