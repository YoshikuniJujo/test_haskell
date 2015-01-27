module StringQQ (string) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

string :: QuasiQuoter
string = QuasiQuoter {
	quoteExp = litE . stringL,
	quotePat = undefined,
	quoteType = undefined,
	quoteDec = undefined }
