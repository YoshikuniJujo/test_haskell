{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.TypeLevel.Uncurry.TH where

import Language.Haskell.TH

v2 :: DecQ
v2 = dataD (cxt []) (ucName 2)
	[	plainTV (mkName "t"), plainTV (mkName "ss") ]
	Nothing
	[	recGadtC
			[ucName 2]
			[	varBangType (ucUnName 2) (
					(,)	<$> bang noSourceUnpackedness noSourceStrictness
						<*> crrType2 )]
			(	conT (mkName "V2") `appT` varT (mkName "t") `appT` ucrrType2 ) ]
	[]

showV2 :: DecQ
showV2 = standaloneDerivD
	(cxt [	conT ''Show `appT` crrType2 ])
	(conT ''Show `appT` (
		conT (ucName 2)
			`appT` varT (mkName "t") `appT` ucrrType2 ))

ucName :: Int -> Name
ucName = mkName . ("V" ++) . show

ucUnName :: Int -> Name
ucUnName = mkName . ("unV" ++) . show

crrType2 :: Q Type
crrType2 = varT (mkName "t")
	`appT` varT (mkName "s1")
	`appT` varT (mkName "s2")

ucrrType2 :: Q Type
ucrrType2 = pTupT2
	(varT $ mkName "s1")
	(varT $ mkName "s2")

t1 `arrT` t2 = arrowT `appT` t1 `appT` t2

pTupT2 t1 t2 = promotedTupleT 2 `appT` t1 `appT` t2
