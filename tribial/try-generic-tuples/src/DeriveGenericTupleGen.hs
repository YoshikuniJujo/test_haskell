{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module DeriveGenericTupleGen where

import GHC.Generics
import Language.Haskell.TH
import Data.Bool

deriveGenericTuple :: Int -> DecsQ
deriveGenericTuple i = newNameAbc i >>= tupT >>= \t ->
	isInstance ''Generic [t] >>= bool
		((: []) <$> standaloneDerivD (cxt [])
			(conT ''Generic `appT` pure t))
		(pure [])

tupT :: [Name] -> TypeQ
tupT = \case
	ns@(_ : _ : _) -> foldl appT (tupleT $ length ns) $ varT <$> ns
	_ -> error "no tuples which has 0 or 1 elements"

newNameAbc :: Int -> Q [Name]
newNameAbc i = newName `mapM` take i infNames

infNames :: [String]
infNames = ((: "") <$> ['a' .. 'z']) ++
	[ x ++ [y] | x <- infNames, y <- ['a' .. 'z'] ]
