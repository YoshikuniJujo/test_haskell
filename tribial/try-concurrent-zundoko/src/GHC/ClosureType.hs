{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.ClosureType (getTipe, isEvaluated) where

import GHC.Exts.Heap (ClosureType(..), getClosureData, tipe, info)

getTipe :: a -> IO ClosureType
getTipe = (tipe . info <$>) . getClosureData

isEvaluated :: a -> IO Bool
isEvaluated x = (<$> getTipe x) $ \case
	BLACKHOLE -> True
	CONSTR -> True
	CONSTR_1_0 -> True; CONSTR_0_1 -> True
	CONSTR_2_0 -> True; CONSTR_1_1 -> True; CONSTR_0_2 -> True
	_ -> False
