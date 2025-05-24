{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.ByteString.Adler32 (

	run_, adler32, adler32',

	A, toWord32, adler32Step

	) where

import Prelude hiding (uncurry)
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.State qualified as State
import Control.HigherOpenUnion qualified as U
import Data.HigherFunctor qualified as HFunctor
import Data.Bits
import Data.Word
import Data.ByteString qualified as BS

run_ :: forall nm es i o r .
	HFunctor.Loose (U.U es) =>
	Eff.E (State.Named nm A ': es) i o r -> Eff.E es i o ()
run_ = void . (`State.runN` A 1 0)

adler32Step :: (Int, A) -> BS.ByteString -> (Int, A)
adler32Step = BS.foldl \(n, A a b) w -> case n of
	0 -> (5551, A ((a + fromIntegral w) `mod` 65521) ((b + a + fromIntegral w) `mod` 65521))
	_ -> (n - 1, A (a + fromIntegral w) (b + a + fromIntegral w))

adler32 :: forall nm -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm A) es ) =>
	Eff.E es BS.ByteString BS.ByteString r
adler32 nm = ($ 5551) $ fix \go n -> do
	bs <- Pipe.await
	a <- State.getN nm
	let	(n', a') = adler32Step (n, a) bs
	State.putN nm a'
	Pipe.yield bs
	go n'

adler32' :: forall nm -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm A) es ) =>
	Eff.E es BS.ByteString BS.ByteString ()
adler32' nm = ($ 5551) $ fix \go n -> Pipe.awaitMaybe >>= \case
	Nothing -> pure ()
	Just bs -> do
		a <- State.getN nm
		let	(n', a') = adler32Step (n, a) bs
		State.putN nm a'
		Pipe.yield bs
		go n'

data A = A Word32 Word32 deriving Show

toWord32 :: A -> Word32
toWord32 = uncurry (.|.) . first (`mod` 65521) . second ((`shiftL` 16) . (`mod` 65521))

first :: (Word32 -> Word32) -> A -> A
first f (A a b) = A (f a) b

second :: (Word32 -> Word32) -> A -> A
second f (A a b) = A a (f b)

uncurry :: (Word32 -> Word32 -> a) -> A -> a
uncurry f (A a b) = f a b
