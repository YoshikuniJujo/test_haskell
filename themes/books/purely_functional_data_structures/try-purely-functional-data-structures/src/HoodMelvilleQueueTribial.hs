{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module HoodMelvilleQueueTribial where

import GHC.Stack (HasCallStack)
import Control.Exception

data ReverseState a = Working [a] [a] | Done [a] deriving Show

startReverse :: [a] -> ReverseState a
startReverse xs = Working xs []

data AlreadyDone = AlreadyDone deriving Show
instance Exception AlreadyDone

exec :: HasCallStack => ReverseState a -> ReverseState a
exec (Done _) = throw AlreadyDone
exec (Working (x : xs) xs') = Working xs (x : xs')
exec (Working [] xs') = Done xs'

data AppendState a
	= Reversing [a] [a] [a]
	| Appending [a] [a]
	| AppendingDone [a]
	deriving Show

startAppend :: [a] -> [a] -> AppendState a
startAppend xs ys = Reversing xs [] ys

exec2 :: HasCallStack => AppendState a -> AppendState a
exec2 (AppendingDone _) = throw AlreadyDone
exec2 (Reversing (x : xs) xs' ys) = Reversing xs (x : xs') ys
exec2 (Reversing [] xs' ys) = Appending xs' ys
exec2 (Appending (x : xs') ys) = Appending xs' (x : ys)
exec2 (Appending [] ys) = AppendingDone ys
