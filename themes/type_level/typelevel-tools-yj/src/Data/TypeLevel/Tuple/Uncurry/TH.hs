{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.TypeLevel.Tuple.Uncurry.TH (uc, showUc) where

import Language.Haskell.TH

uc :: Int -> DecQ
uc n = do
	t <- mkT
	ss <- mkSs n
	dataD (cxt []) (ucName n)
		[	plainTV (mkName "t"), plainTV (mkName "ss") ]
		Nothing
		[	recGadtC
				[ucName n]
				[	varBangType (ucUnName n) (
						(,)	<$> noSourceBang
							<*> crrType t ss)]
				(	conT (ucName n)
						`appT` varT t
						`appT` ucrrType ss ) ]
		[]

noSourceBang :: BangQ
noSourceBang = bang noSourceUnpackedness noSourceStrictness

showUc :: Int -> DecQ
showUc n = do
	t <- mkT
	ss <- mkSs n
	standaloneDerivD
		(cxt [	conT ''Show `appT` crrType t ss ])
		(conT ''Show `appT` (
			conT (ucName n)
				`appT` varT t `appT` ucrrType ss ))

ucName :: Int -> Name
ucName = mkName . ("U" ++) . show

ucUnName :: Int -> Name
ucUnName = mkName . ("unU" ++) . show

mkT :: Q Name
mkT = newName "t"

mkSs :: Int -> Q [Name]
mkSs n = (newName . ("s" ++) . show) `mapM` [1 .. n]

crrType :: Name -> [Name] -> TypeQ
crrType t = foldl appT (varT t) . map varT -- varT t `appT` varT (mkName "s1") `appT` varT (mkName "s2")

ucrrType :: [Name] -> Q Type
ucrrType = pTupTN -- pTupT2 (varT $ mkName "s1") (varT $ mkName "s2")

pTupTN :: [Name] -> TypeQ
pTupTN ns = foldl appT (promotedTupleT $ length ns) $ map varT ns
