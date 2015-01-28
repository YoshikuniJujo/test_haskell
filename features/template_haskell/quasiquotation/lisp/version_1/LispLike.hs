module LispLike (lisp) where

import Language.Haskell.TH.Quote

import Parser

lisp :: QuasiQuoter
lisp = QuasiQuoter {
	quoteExp = fst . parseExp . lexer,
	quotePat = undefined,
	quoteType = undefined,
	quoteDec = parseDec . lexer }
