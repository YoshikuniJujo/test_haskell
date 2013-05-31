{-# LANGUAGE QuasiQuotes, FlexibleContexts #-}

module Type (Def(..), Lambda(..)) where

data Def = Def { defVar :: String,  defBody :: Lambda } deriving Show
data Lambda
	= Int Int
	| Var String
	| Apply Lambda Lambda
	| Lambda String Lambda
	deriving Show
