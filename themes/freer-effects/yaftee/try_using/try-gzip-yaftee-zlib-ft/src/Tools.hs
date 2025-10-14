{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE FlexibleContexts #-}

module Tools where

import Control.Arrow
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.HigherOpenUnion qualified as U

import Control.Monad.Primitive
import Data.Bits
import Data.Bool
import Data.Word
import Data.Color
import Data.Image.Simple qualified as Image

pipeZip :: U.Member Pipe.P es => [b] -> Eff.E es [a] [(a, b)] ()
pipeZip = \case
	[] -> pure ()
	ys -> do
		xs <- Pipe.await
		let	(xys, (_xs', ys')) = xs `zip'` ys
		Pipe.yield xys
		pipeZip ys'

zip' :: [a] -> [b] -> ([(a, b)], ([a], [b]))
zip' xs [] = ([], (xs, []))
zip' [] ys = ([], ([], ys))
zip' (x : xs) (y : ys) = ((x, y) :) `first` zip' xs ys

fromImage :: forall m -> (
	PrimMonad m, RealFrac d,
	U.Member Pipe.P es,
	U.Base (U.FromFirst m) es
	) =>
	Image.I (PrimState m) -> [[(Int, Int)]] ->
	Eff.E es i [Rgba d] ()
fromImage m img = \case
	[] -> pure ()
	ps : pss -> do
		Pipe.yield =<< (\(x, y) -> Eff.effBase $ Image.read @m img x y) `mapM` ps
		fromImage m img pss

fromImageGray :: forall m -> (
	PrimMonad m,
	U.Member Pipe.P es,
	U.Base (U.FromFirst m) es ) =>
	Image.Gray (PrimState m) -> [[(Int, Int)]] -> Eff.E es i [Word8] ()
fromImageGray m img = \case
	[] -> pure ()
	ps : pss -> do
		Pipe.yield =<< (\(x, y) -> Eff.effBase $ Image.grayRead @m img x y) `mapM` ps
		fromImageGray m img pss

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
