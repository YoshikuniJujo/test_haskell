module Text (Text(..), List, List1(..), addChapters) where

import Control.Applicative ((<$>))
import Data.Maybe
import Data.List

data Text
	= Header Int String
	| Paras [String]
	| Code String
	| List List
	deriving Show

type List = [List1]
data List1 = OrdItem String List | BulItem String List deriving Show

addChapters :: [Maybe Int] -> [Text] -> [Text]
addChapters _ [] = []
addChapters cs (Header n s : ts)
	| isJust $ cs !! (n - 1) =
		Header n (chaps ++ " " ++ s) : addChapters newCs ts
	| otherwise = Header n s : addChapters newCs ts
	where
	chaps = concatMap (++ ".") $ map show $ catMaybes $ take n newCs
	newCs = take (n - 1) cs ++ [(+ 1) <$> cs !! (n - 1)] ++ drop n cs
addChapters cs (t : ts) = t : addChapters cs ts
