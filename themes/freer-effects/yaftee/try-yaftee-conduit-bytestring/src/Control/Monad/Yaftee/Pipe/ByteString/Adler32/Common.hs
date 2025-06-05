{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.ByteString.Adler32.Common (

	run_,

	A(..), toWord32,

	) where

import Prelude hiding (uncurry)
import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.State qualified as State
import Control.HigherOpenUnion qualified as U
import Data.HigherFunctor qualified as HFunctor
import Data.Bits
import Data.Word

run_ :: forall nm es i o r .
	HFunctor.Loose (U.U es) =>
	Eff.E (State.Named nm A ': es) i o r -> Eff.E es i o ()
run_ = void . (`State.runN` A 1 0)

data A = A !Word32 !Word32 deriving Show

toWord32 :: A -> Word32
toWord32 = uncurry (.|.) . first (`mod` 65521) . second ((`shiftL` 16) . (`mod` 65521))

first :: (Word32 -> Word32) -> A -> A
first f (A a b) = A (f a) b

second :: (Word32 -> Word32) -> A -> A
second f (A a b) = A a (f b)

uncurry :: (Word32 -> Word32 -> a) -> A -> a
uncurry f (A a b) = f a b
