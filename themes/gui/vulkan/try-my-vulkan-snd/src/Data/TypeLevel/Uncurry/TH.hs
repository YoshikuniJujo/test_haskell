{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.TypeLevel.Uncurry.TH where

import Language.Haskell.TH

v2 :: DecQ
v2 = dataD (cxt []) (mkName "V2")
	[	plainTV (mkName "t"), plainTV (mkName "ss") ]
	Nothing
	[	recGadtC
			[mkName "V2"]
			[	varBangType (mkName "unV2") (
					(,)	<$> bang noSourceUnpackedness noSourceStrictness
						<*> varT (mkName "t") `appT` varT (mkName "s1") `appT` varT (mkName "s2")
					)]
			(	conT (mkName "V2") `appT` varT (mkName "t") `appT` pTupT2 (varT $ mkName "s1") (varT $ mkName "s2")
				) ]
	[]

t1 `arrT` t2 = arrowT `appT` t1 `appT` t2

pTupT2 t1 t2 = promotedTupleT 2 `appT` t1 `appT` t2

showV2 :: DecQ
showV2 = standaloneDerivD
	(cxt [	conT ''Show `appT` (
			varT (mkName "t")
				`appT` varT (mkName "s1")
				`appT` varT (mkName "s2") )
		])
	(conT ''Show `appT` (
		conT (mkName "V2")
			`appT` varT (mkName "t")
			`appT` pTupT2
				(varT $ mkName "s1")
				(varT $ mkName "s2")
		))
