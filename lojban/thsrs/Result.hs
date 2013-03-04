module Result (
	Result,
	resultToXml
) where

import Data.Tree

type Result = Tree (String, [(String, String)])

resultToXml :: [Result] -> String
resultToXml secs =
	"<?xml version=\"1.0\" ?>\n" ++
	"<top>" ++ concatMap rtx secs ++ "</top>"

rtx :: Result -> String
rtx (Node (title, ws) secs) =
	"<section title=\"" ++ title ++ "\">\n" ++
	unlines (map wordsToXml ws) ++
	concatMap rtx secs ++ "</section>"

wordsToXml (loj, en) =
	"<item title=\"" ++ loj ++ "\" description=\"" ++ en ++ "\"/>"
