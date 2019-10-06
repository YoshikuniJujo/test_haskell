{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module FingerTree.TH where

import Control.Arrow
import Language.Haskell.TH

instanceFoldable :: Name -> [(Name, Int)] -> DecsQ
instanceFoldable i cs = (: []) <$> instanceFoldableGen (conT i) (first conP <$> cs)

type ConNum = ([PatQ] -> PatQ, Int)

instanceFoldableGen :: TypeQ -> [ConNum] -> DecQ
instanceFoldableGen i cs = do
	instanceD (pure []) (conT ''Foldable `appT` i) [
		funD 'foldr (foldrDec <$> cs),
		funD 'foldl (foldlDec <$> cs) ]

foldrDec, foldlDec :: ConNum -> ClauseQ
foldrDec c1 = do
	op <- newName ">-"
	z <- newName "z"
	args <- newName `mapM` ((: []) <$> take (snd c1) ['a' ..])
	clause	[ varP op, varP z, fst c1 $ varP <$> args]
		(normalB . appR op z $ take (snd c1) args)
		[]

foldlDec c1 = do
	op <- newName ">-"
	z <- newName "z"
	args <- newName `mapM` ((: []) <$> take (snd c1) ['a' ..])
	clause	[ varP op, varP z, fst c1 $ varP <$> args]
		(normalB . appL op z $ take (snd c1) args)
		[]

appR :: Name -> Name -> [Name] -> ExpQ
appR op z vs = foldr f (varE z) vs
	where f e v = infixE (Just $ varE e) (varE op) (Just v)

appL :: Name -> Name -> [Name] -> ExpQ
appL op z vs = foldl f (varE z) vs
	where f v e = infixE (Just v) (varE op) (Just $ varE e)
