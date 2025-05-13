{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.Bits where

import Control.Arrow
import Control.Monad.Fix
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.State qualified as State
import Control.HigherOpenUnion qualified as U
import Data.Bool
import Data.ByteString qualified as BS
import Data.ByteString.Bit (pattern O)
import Data.ByteString.Bit qualified as Bit

toByteString :: (U.Member Pipe.P es, U.Member (State.S Bit.Queue) es) =>
	Eff.E es [Bit.B] BS.ByteString r
toByteString = fix \go -> (>> go) do
	State.modify . flip Bit.append =<< Pipe.await
	Pipe.yield =<< uncurry (<$)
		. (BS.pack *** State.put) . unfoldr' Bit.popByte =<< State.get

toByteString' :: (U.Member Pipe.P es, U.Member (State.S Bit.Queue) es) =>
	Eff.E es [Bit.B] BS.ByteString ()
toByteString' = fix \go -> Pipe.isMore >>= bool
	do	State.modify (`Bit.append` [O, O, O, O, O, O, O])
		Pipe.yield =<< uncurry (<$)
			. (BS.pack *** State.put)
			. unfoldr' Bit.popByte =<< State.get
	do	State.modify . flip Bit.append =<< Pipe.await
		Pipe.yield =<< uncurry (<$)
			. (BS.pack *** State.put)
			. unfoldr' Bit.popByte =<< State.get
		go
	

unfoldr' :: (b -> Maybe (a, b)) -> b -> ([a], b)
unfoldr' f = fix \go s -> maybe ([], s) (\(x, s') -> (x :) `first` go s') $ f s
