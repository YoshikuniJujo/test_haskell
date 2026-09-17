{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-x-partial #-}

module Main where

import Control.Arrow
import System.Environment

main :: IO ()
main = do
	[fp] <- getArgs
	print . check =<< readFile fp

filterVoid :: [((Int, Int), String)] -> [((Int, Int), String)]
filterVoid = filter $ (`notElem` ["!doctype", "meta", "link", "input", "hr"]) . snd

getOpens :: (Int, Int) -> String -> [((Int, Int), String)]
getOpens (l, c) ('<' : str) = getClose (l, c) (l, c + 1) "" str
getOpens (l, _) ('\n' : str) = getOpens (l + 1, 0) str
getOpens (l, c) ('\t' : str) = getOpens (l, c + 8) str
getOpens (l, c) (_ : str) = getOpens (l, c + 1) str
getOpens _ "" = []

getClose :: (Int, Int) -> (Int, Int) -> String -> String -> [((Int, Int), String)]
getClose (l0, c0) (l, c) s ('>' : str) = ((l0, c0), head . words $ reverse s) : getOpens (l, c + 1) str
getClose (l0, c0) (l, _) s ('\n' : str) = getClose (l0, c0) (l + 1, 0) ('\n' : s) str
getClose (l0, c0) (l, c) s (chr : str) = getClose (l0, c0) (l, c + 1) (chr : s) str
getClose _ _ _ [] = error "not closed tag"

data Tag = OpenTag String | CloseTag String deriving Show

toTag :: String -> Tag
toTag ('/' : nm) = CloseTag nm
toTag nm = OpenTag nm

tagMatch :: [((Int, Int), String)] -> [((Int, Int), Tag)] -> Maybe (((Int, Int), String), ((Int, Int), String))
tagMatch os ((ps, OpenTag nm ) : ts) = tagMatch ((ps, nm) : os) ts
tagMatch ((ops, onm) : os) ((ps, CloseTag nm) : ts)
	| onm == nm = tagMatch os ts
	| otherwise = Just ((ops, onm), (ps, nm))
tagMatch [] [] = Nothing
tagMatch os ts = error $ show os ++ " " ++ show ts

check :: String -> Maybe (((Int, Int), String), ((Int, Int), String))
check = tagMatch [] . ((toTag `second`) <$>) . filterVoid . getOpens (1, 0)
