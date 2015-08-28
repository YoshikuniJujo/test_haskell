import Language.Haskell.TH

sel :: Int -> Int -> ExpQ
sel n i = do
	x <- newName "x"
	lamE [tupP $ bws ++ [varP x] ++ aws] $ varE x
	where
	bws = replicate i wildP
	aws = replicate (n - i - 1) wildP
