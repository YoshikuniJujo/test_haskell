{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tools (

	div', boolsToWords, pop

	) where

import GHC.TypeLits

import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.State qualified as State
import Control.HigherOpenUnion qualified as U

import Data.Bits
import Data.Bool
import Data.Word

boolsToWords :: [Bool] -> [Word8]
boolsToWords = (boolsToWord <$>) . sep 8

boolsToWord :: [Bool] -> Word8
boolsToWord = go 0 . to8
	where
	go r [] = r
	go r (b : bs) = go (bool id (.|. 1) b (r `shiftL` 1)) bs
	to8 bs = bs ++ replicate (8 - Prelude.length bs) False

sep :: Int -> [a] -> [[a]]
sep _ [] = []
sep n xs = take n xs : sep n (drop n xs)

div' :: Integral n => n -> n -> n
m `div'`n = (m - 1) `div` n + 1

pop :: forall (nm :: Symbol) ->
	(U.Member (State.Named nm [a]) es) => Eff.E es i o (Maybe a)
pop nm = State.getsModifyN nm \case [] -> Nothing; x : xs -> Just (x, xs)
