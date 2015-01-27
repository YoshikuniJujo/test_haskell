module ListQQ (list) where

import Control.Applicative
import Language.Haskell.TH
import Language.Haskell.TH.Quote

list :: QuasiQuoter
list = QuasiQuoter {
	quoteExp = listE . map (litE . stringL) . words,
	quotePat = listP . map (varP . mkName) . words,
	quoteType = (listT `appT`) . conT . mkName,
	quoteDec = \s -> (: []) <$> valD
		(varP $ mkName "hoge")
		(normalB (listE . map (litE . stringL) $
			words s))
		[] }
