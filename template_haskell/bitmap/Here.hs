{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Here (printf, here) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

printf :: QuasiQuoter
printf = QuasiQuoter {
	quoteExp = buildFun . parsePrintf,
	quotePat = undefined,
	quoteType = undefined,
	quoteDec = undefined
 }

data Printf
	= Lit Char
	| String
	| Dec
	| Float
	| Show
	| Char
	deriving (Show, Eq)

parsePrintf :: String -> [Printf]
parsePrintf (c : _) = [Lit c]

buildFun :: [Printf] -> ExpQ
buildFun [Lit c] = [| putChar c |]

here :: QuasiQuoter
here = QuasiQuoter {
	quoteExp = litE . stringL . tail,
	quotePat = undefined,
	quoteType = undefined,
	quoteDec = undefined
 }
