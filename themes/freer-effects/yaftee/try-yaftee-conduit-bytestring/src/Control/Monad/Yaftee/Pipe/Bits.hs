{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.Bits (

	toByteString, toByteString', Queue, empty

	) where

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

toByteString :: forall nm -> (U.Member Pipe.P es, U.Member (State.Named nm Queue) es) =>
	Eff.E es [Bit.B] BS.ByteString r
toByteString nm = fix \go -> (>> go) do
	State.modifyN nm . flip append =<< Pipe.await
	Pipe.yield =<< uncurry (<$)
		. (BS.pack *** State.putN nm . Queue) . unfoldr' Bit.popByte =<< State.getsN nm unQueue

toByteString' :: forall nm -> (U.Member Pipe.P es, U.Member (State.Named nm Queue) es) =>
	Eff.E es [Bit.B] BS.ByteString ()
toByteString' nm = fix \go -> Pipe.isMore >>= bool
	do	State.modifyN nm $ Queue . (`Bit.append` [O, O, O, O, O, O, O]) . unQueue
		Pipe.yield =<< uncurry (<$)
			. (BS.pack *** State.putN nm . Queue)
			. unfoldr' Bit.popByte =<< State.getsN nm unQueue
	do	State.modifyN nm . flip append =<< Pipe.await
		Pipe.yield =<< uncurry (<$)
			. (BS.pack *** State.putN nm . Queue)
			. unfoldr' Bit.popByte =<< State.getsN nm unQueue
		go
	

unfoldr' :: (b -> Maybe (a, b)) -> b -> ([a], b)
unfoldr' f = fix \go s -> maybe ([], s) (\(x, s') -> (x :) `first` go s') $ f s

newtype Queue = Queue { unQueue :: Bit.Queue } deriving Show

empty :: Queue
empty = Queue Bit.empty

append :: Queue -> [Bit.B] -> Queue
append (Queue q) bs = Queue $ q `Bit.append` bs
