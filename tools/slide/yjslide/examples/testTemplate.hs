{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH

main = return ()

do	l <- location
	runIO $ print (
		loc_filename l,
		loc_package l,
		loc_module l,
		loc_start l,
		loc_end l)
	return []
