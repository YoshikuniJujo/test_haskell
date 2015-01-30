module LispLike (lisp) where

import Control.Applicative
import Language.Haskell.TH.Quote

import Parser
import AddForallT

lisp :: QuasiQuoter
lisp = QuasiQuoter {
	quoteExp = fst . parseExp . lexer,
	quotePat = fst . parsePat . lexer,
	quoteType = (addForallT <$>) .
		fst . parseType . lexer,
	quoteDec = parseDec . lexer }
