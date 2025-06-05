{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.ByteString.Lazy.Adler32 (

	run_, adler32, adler32',

	A, toWord32, adler32Step

	) where

import Prelude hiding (uncurry)
import Control.Monad.Fix
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.ByteString.Adler32.Common
import Control.Monad.Yaftee.State qualified as State
import Control.HigherOpenUnion qualified as U
import Data.ByteString.Lazy qualified as LBS

adler32Step :: (Int, A) -> LBS.ByteString -> (Int, A)
adler32Step = LBS.foldl \(n, A a b) w -> case n of
	0 -> (5551, A ((a + fromIntegral w) `mod` 65521) ((b + a + fromIntegral w) `mod` 65521))
	_ -> (n - 1, A (a + fromIntegral w) (b + a + fromIntegral w))

adler32 :: forall nm -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm A) es ) =>
	Eff.E es LBS.ByteString LBS.ByteString r
adler32 nm = ($ 5551) $ fix \go n -> do
	bs <- Pipe.await
	a <- State.getN nm
	let	!(!n', !a') = adler32Step (n, a) bs
	State.putN nm a'
	Pipe.yield bs
	go n'

adler32' :: forall nm -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm A) es ) =>
	Eff.E es LBS.ByteString LBS.ByteString ()
adler32' nm = ($ 5551) $ fix \go n -> Pipe.awaitMaybe >>= \case
	Nothing -> pure ()
	Just bs -> do
		a <- State.getN nm
		let	(n', a') = adler32Step (n, a) bs
		State.putN nm a'
		Pipe.yield bs
		go n'
