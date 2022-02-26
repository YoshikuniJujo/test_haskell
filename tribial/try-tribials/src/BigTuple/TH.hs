{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module BigTuple.TH where

import Language.Haskell.TH

tuple3Data :: Q Dec
tuple3Data = dataD (cxt []) (mkName "Tuple3")
	[plainTV $ mkName "a", plainTV $ mkName "b", plainTV $ mkName "c"]
	Nothing
	[normalC (mkName "Tuple3") [
		bangType (bang noSourceUnpackedness noSourceStrictness) (varT $ mkName "a"),
		bangType (bang noSourceUnpackedness noSourceStrictness) (varT $ mkName "b"),
		bangType (bang noSourceUnpackedness noSourceStrictness) (varT $ mkName "c") ]]
	[]

tuple3Type :: TypeQ
tuple3Type = conT (mkName "Tuple3")
	`appT` conT ''Int `appT` conT ''Char `appT` conT ''()

tuple3Pat :: PatQ
tuple3Pat = conP (mkName "Tuple3") [
	varP $ mkName "x", varP $ mkName "y", varP $ mkName "z" ]

tuple3Exp :: ExpQ
tuple3Exp = conE (mkName "Tuple3")
	`appE` litE (integerL 1) `appE` litE (charL 'c') `appE` tupE []
