{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Reimplementation.Queue (Queue(..), isEmpty, head, tail) where

import Prelude hiding (head, tail)

import GHC.Stack (HasCallStack)
import Control.Exception (Exception, throw)
import Data.Maybe (isNothing)

class Queue q where
	empty :: q a
	snoc :: q a -> a -> q a
	uncons :: q a -> Maybe (a, q a)

isEmpty :: Queue q => q a -> Bool
isEmpty = isNothing . uncons

data Empty = Empty deriving Show

instance Exception Empty

head :: (HasCallStack, Queue q) => q a -> a
head = maybe (throw Empty) fst . uncons

tail :: (HasCallStack, Queue q) => q a -> q a
tail = maybe (throw Empty) snd . uncons
