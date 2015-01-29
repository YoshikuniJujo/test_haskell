module LispLike (lisp) where

import Language.Haskell.TH.Quote

import Parser

lisp :: QuasiQuoter
lisp = QuasiQuoter {
	quoteExp = fst . parseExp . lexer,
	quotePat = fst . parsePat . lexer,
	quoteType = fst . parseType . lexer,
	quoteDec = parseDec . lexer }
