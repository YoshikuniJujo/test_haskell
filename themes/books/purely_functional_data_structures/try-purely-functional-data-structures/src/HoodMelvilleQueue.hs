{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module HoodMelvilleQueue where

import GHC.Stack (HasCallStack)
import Control.Exception

data AlreadyDone = AlreadyDone deriving Show
instance Exception AlreadyDone

data FormatError = FormatError deriving Show
instance Exception FormatError

data RotationState a
	= Reversing Int [a] [a] [a] [a]
	| Appending Int [a] [a]
	| Done [a]
	deriving Show

startRotation :: [a] -> [a] -> RotationState a
startRotation f r = Reversing 0 f [] r []

exec :: HasCallStack => RotationState a -> RotationState a
exec (Done _) = throw AlreadyDone
exec (Reversing ok (x : f) f' (y : r) r') =
	Reversing (ok + 1) f (x : f') r (y : r')
exec (Reversing ok [] f' [y] r') = Appending ok f' (y : r')
exec (Appending 0 _ r') = Done r'
exec (Appending ok (x : f') r') = Appending (ok - 1) f' (x : r')
exec _ = throw FormatError

invalidate :: HasCallStack => RotationState a -> RotationState a
invalidate (Done _) = throw AlreadyDone
invalidate (Reversing ok f f' r r') = Reversing (ok - 1) f f' r r'
invalidate (Appending 0 _ (_ : r')) = Done r'
invalidate (Appending ok f' r') = Appending (ok - 1) f' r'
