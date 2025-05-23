{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.ByteString.Adler32 (
	run_, adler32, adler32', A, toWord32, adler32Step
	) where

import Prelude hiding (uncurry)
import Control.Monad
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

adler32Step :: A -> BS.ByteString -> A
adler32Step = BS.foldl \(A a b) w ->
	A ((a + fromIntegral w) `mod` 65521) ((b + a + fromIntegral w) `mod` 65521)

adler32 :: forall nm -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm A) es ) =>
	Eff.E es BS.ByteString BS.ByteString r
adler32 nm = forever do
	bs <- Pipe.await
	State.modifyN nm (`adler32Step` bs)
	Pipe.yield bs

adler32' :: forall nm -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm A) es ) =>
	Eff.E es BS.ByteString BS.ByteString ()
adler32' nm = Pipe.awaitMaybe >>= \case
	Nothing -> pure ()
	Just bs -> do
		State.modifyN nm (`adler32Step` bs)
		Pipe.yield bs
		adler32' nm

data A = A Word32 Word32 deriving Show

toWord32 = uncurry (.|.) . second (`shiftL` 16)

second f (A a b) = A a (f b)

uncurry :: (Word32 -> Word32 -> a) -> A -> a
uncurry f (A a b) = f a b
