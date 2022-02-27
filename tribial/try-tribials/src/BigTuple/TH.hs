{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module BigTuple.TH where

import Language.Haskell.TH

tuple3Data :: Int -> Q Dec
tuple3Data n = dataD (cxt []) (mkName "Tuple3")
	(plainTV . mkName <$> as)
	Nothing
	[normalC (mkName "Tuple3")
		$ bangType (bang noSourceUnpackedness noSourceStrictness)
			. varT . mkName <$> as]
	[]
	where as = take n abc

abc :: [String]
abc = ((: []) <$> ['a' .. 'z']) ++ [ as ++ [a] | as <- abc, a <- ['a' .. 'z'] ]

tuple3Type :: [TypeQ] -> TypeQ
tuple3Type = foldl appT (conT $ mkName "Tuple3")

tuple3Pat :: [PatQ] -> PatQ
tuple3Pat = conP (mkName "Tuple3")

tuple3Exp :: ExpQ
tuple3Exp = conE (mkName "Tuple3")
