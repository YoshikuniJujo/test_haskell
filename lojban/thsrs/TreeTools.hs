module TreeTools(showTreeIndented, printTreeIndented) where

import Data.Tree
import Data.List

testTree = Node "section" [
	Node "subsection1" [Node "subsubsection" []],
	Node "subsection2" [Node "subsubsection2-1" []]
 ]

showTreeIndented :: (Int -> a -> String) -> Int -> Tree a -> String
showTreeIndented sh ind (Node v s) =
	sh ind v ++ concatMap (showTreeIndented sh $ ind + 1) s

printTreeIndented :: Show a => Tree a -> IO ()
printTreeIndented = putStr . showTreeIndented sh 0
	where
	sh i x = replicate (i * 4) ' ' ++ show x ++ "\n"
