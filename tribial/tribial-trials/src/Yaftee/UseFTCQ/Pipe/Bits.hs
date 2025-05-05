{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Yaftee.UseFTCQ.Pipe.Bits (

	-- * TYPE

	Bit.B(..), Bit.Queue, Bit.listFromNum,

	-- * PIPE

	toByteString

	) where

import Control.Arrow
import Control.Monad.Fix
import Data.Bit qualified as Bit
import Data.ByteString qualified as BS

import Yaftee.UseFTCQ.Eff qualified as Eff
import Yaftee.UseFTCQ.Pipe qualified as Pipe
import Yaftee.UseFTCQ.State qualified as State
import Yaftee.OpenUnion qualified as Union

toByteString ::
	(Union.Member Pipe.P es, Union.Member (State.S Bit.Queue) es) =>
	Eff.E es [Bit.B] BS.ByteString r
toByteString = fix \go -> (>> go) do
	State.modify . flip Bit.append =<< Pipe.await
	Pipe.yield =<< uncurry (<$)
		. (BS.pack *** State.put) . unfoldr' Bit.popByte =<< State.get

unfoldr' :: (b -> Maybe (a, b)) -> b -> ([a], b)
unfoldr' f = fix \go s -> maybe ([], s) (\(x, s') -> (x :) `first` go s') $ f s
