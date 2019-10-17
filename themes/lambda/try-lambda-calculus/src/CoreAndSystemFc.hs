{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CoreAndSystemFc where

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.String

data Var = X | Y | Z | W | Primed Var deriving Show

data Con = Con deriving Show

data VarCon = V Var | C Con deriving Show

data Expr
	= VarCon VarCon
	deriving Show

instance Pretty Var where
	pretty X = "x"
	pretty Y = "y"
	pretty Z = "z"
	pretty W = "w"
	pretty (Primed v) = pretty v <> "'"

showPretty :: Pretty a => a -> String
showPretty = renderString . layoutCompact . pretty

putPretty :: Pretty a => a -> IO ()
putPretty = putStrLn . showPretty
