{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tools (div', boolsToWords, pop) where

import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.State qualified as State
import Control.HigherOpenUnion qualified as U
import Data.Bits
import Data.Bool
import Data.Word

div' :: Integral n => n -> n -> n
m `div'`n = (m - 1) `div` n + 1

boolsToWords :: [Bool] -> [Word8]
boolsToWords = (btw 0 . to8 <$>) . sep 8
	where
	btw r = \case
		[] -> r
		b : bs -> btw (bool id (.|. 1) b (r `shiftL` 1)) bs
	to8 bs = bs ++ replicate (8 - length bs) False
	sep n = \case [] -> []; xs -> take n xs : sep n (drop n xs)

pop :: forall nm -> (U.Member (State.Named nm [a]) es) => Eff.E es i o (Maybe a)
pop nm = State.getsModifyN nm \case [] -> Nothing; x : xs -> Just (x, xs)
