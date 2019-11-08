{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module HoodMelvilleQueue where

import GHC.Stack (HasCallStack)
import Control.Exception

data AlreadyDone = AlreadyDone deriving Show
instance Exception AlreadyDone

data FormatError = FormatError deriving Show
instance Exception FormatError

data RotationState a
	= Idle
	| Reversing Int [a] [a] [a] [a]
	| Appending Int [a] [a]
	| Done [a]
	deriving Show

startRotation :: [a] -> [a] -> RotationState a
startRotation f r = Reversing 0 f [] r []

exec :: RotationState a -> RotationState a
exec (Done _) = throw AlreadyDone
exec (Reversing ok (x : f) f' (y : r) r') =
	Reversing (ok + 1) f (x : f') r (y : r')
exec (Reversing ok [] f' [y] r') = Appending ok f' (y : r')
exec (Appending 0 _ r') = Done r'
exec (Appending ok (x : f') r') = Appending (ok - 1) f' (x : r')
exec st = st

invalidate :: HasCallStack => RotationState a -> RotationState a
invalidate (Done _) = throw AlreadyDone
invalidate (Reversing ok f f' r r') = Reversing (ok - 1) f f' r r'
invalidate (Appending 0 _ (_ : r')) = Done r'
invalidate (Appending ok f' r') = Appending (ok - 1) f' r'
invalidate st = st

data Queue a = Queue Int [a] (RotationState a) Int [a] deriving Show

exec2 :: Queue a -> Queue a
exec2 (Queue lenf f state lenr r) = case exec $ exec state of
	Done newf -> Queue lenf newf Idle lenr r
	newstate -> Queue lenf f newstate lenr r

check :: Queue a -> Queue a
check q@(Queue lenf f _ lenr r)
	| lenr <= lenf = exec2 q
	| otherwise = let newstate = Reversing 0 f [] r [] in
		exec2 $ Queue (lenf + lenr) f newstate 0 []

empty :: Queue a
empty = Queue 0 [] Idle 0 []

isEmpty :: Queue a -> Bool
isEmpty (Queue 0 _ _ _ _) = True
isEmpty _ = False

snoc :: Queue a -> a -> Queue a
snoc (Queue lenf f state lenr r) x = check $ Queue lenf f state (lenr + 1) (x : r)

data Empty = Empty deriving Show
instance Exception Empty

head :: HasCallStack => Queue a -> a
head (Queue 0 [] _ _ _) = throw Empty
head (Queue _ (x : _) _ _ _) = x
head _ = throw FormatError

tail :: HasCallStack => Queue a -> Queue a
tail (Queue 0 [] _ _ _) = throw Empty
tail (Queue lenf (_ : f) state lenr r) =
	check $ Queue (lenf - 1) f (invalidate state) lenr r
tail _ = throw FormatError

uncons :: HasCallStack => Queue a -> Maybe (a, Queue a)
uncons (Queue 0 [] _ _ _) = Nothing
uncons (Queue lenf (x : f) state lenr r) =
	Just (x, check $ Queue (lenf - 1) f (invalidate state) lenr r)
uncons _ = throw FormatError
