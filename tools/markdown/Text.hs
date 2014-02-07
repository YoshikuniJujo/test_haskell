module Text (Text(..), List, List1(..)) where

data Text
	= Header Int String
	| Paras [String]
	| Code String
	| List List
	deriving Show

type List = [List1]
data List1 = OrdItem String List | BulItem String List deriving Show
