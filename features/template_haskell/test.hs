{-# LANGUAGE TemplateHaskell #-}

import Control.Applicative
import Language.Haskell.TH

sayHello = $(appE (varE 'print) (litE (stringL "hello")))

(: []) <$> valD (varP $ mkName "three") (normalB . litE $ integerL 3) []

$(do	t <- reify $ mkName "three"
	f <- reify $ mkName "three"
	n1 <- newName "hoge"
	n2 <- newName "hoge"
	n3 <- newName "hige"
	runIO $ print t
	runIO $ print f
	runIO $ print n1
	runIO $ print n2
	runIO $ print n3
	return [])

(: []) <$> valD (varP $ mkName "four") (normalB . litE $ integerL 4) []

sel :: Int -> Int -> ExpQ
sel n i = do
	x <- newName "x"
	lamE [tupP $ bws ++ [varP x] ++ aws] $ varE x
	where
	bws = replicate i wildP
	aws = replicate (n - i - 1) wildP
