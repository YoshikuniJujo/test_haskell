{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeOperators, FlexibleInstances, FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Foreign.Ptr
import Foreign.Marshal

import HeteroList

allocaIntBool :: (Ptr Int -> Ptr Bool -> IO a) -> IO a
allocaIntBool act = alloca $ \pint -> alloca $ \pb -> act pint pb

sample :: Int -> Bool -> IO (Int, Bool)
sample i b = allocaIntBool $ \pint pb -> do
	pokeHeteroList (pint :-- pb :-- PtrNil) (i :- b :- Nil)
	i' :- b' :- Nil <- peekHeteroList (pint :-- pb :-- PtrNil)
	return (i', b')
