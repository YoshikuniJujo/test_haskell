{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module AnotherUntypedLambdaCalculus where

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.String

data V = X | Primed V deriving Show

data LambdaTerm
	= Var V | Lambda V LambdaTerm | LambdaTerm :$ LambdaTerm deriving Show

instance Pretty V where
	pretty X = "x"
	pretty (Primed v) = pretty v <> "'"

instance Pretty LambdaTerm where
	pretty (Var v) = pretty v
	pretty (Lambda v m) = parens $ "λ" <> pretty v <> "." <> pretty m
	pretty (m :$ n) = parens $ pretty m <> pretty n

showPretty :: Pretty a => a -> String
showPretty = renderString . layoutCompact . pretty

putPretty :: Pretty a => a -> IO ()
putPretty = putStrLn . showPretty
