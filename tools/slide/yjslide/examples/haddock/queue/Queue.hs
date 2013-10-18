{-# LANGUAGE ViewPatterns #-}

-- |For test of haddock.
--
-- The documentation of haddock is here.
-- <http://www.haskell.org/haddock/doc/html/index.html>
module Queue (
	-- * Classes
	Queue(..),
	-- * Types
	TwoList(..),
	-- * Functions
	putQueue,
	putQueue',
	-- * Examples
	hello
) where

import Control.Monad

-- |The queue is FIFO container.
-- The /oldest/ entry is processed /first/.
class Queue q where
	empty :: q a
	-- ^ To use as initial queue.
	enqueue ::
		a {- ^ The @object@ which is put in the @queue@. -} ->
		q a {- ^ The @queue@ which the @object@ put in. -} ->
		q a {- ^ The @result@ of putting in. -}
	-- ^ To put something in the queue.
	dequeue ::
		q a {- ^ The @queue@ which you get a entry from. -} ->
		Maybe (a, q a) {- ^ The result tuple.
					First is a @gotten entry@.
					Second is @the queue of the rest@. -}
	-- ^ To get /oldest/ entry from the queue.

-- |TwoList is made of two list.
data TwoList a = TwoList [a] [a] -- ^ The constructor put two list in one.
	deriving Show

-- |TwoList can be used as queue.
instance Queue TwoList where
	empty = TwoList [] []
	enqueue x (TwoList es ds) = TwoList (x : es) ds
	dequeue (TwoList [] []) = Nothing
	dequeue (TwoList es []) = dequeue (TwoList [] $ reverse es)
	dequeue (TwoList es (d : ds)) = Just (d, TwoList es ds)

-- |Sample queue. \'h', \'e', \'l', \'l', \'o' and '\n' was enqueued in that order.
--
-- >>> hello :: TwoList Char
-- TwoList "\nolleh" ""
--
-- >>> dequeue (hello :: TwoList Char)
-- Just ('h',TwoList "" "ello\n")
--
hello :: Queue q => q Char
hello = foldl (flip enqueue) empty "hello\n"

-- |To print dequed characters until the queue is empty.
putQueue, putQueue' :: Queue q =>
	q Char	{- ^ The @queue@ whose element will be printed. -} ->
	IO ()	{- ^ The printing @action@. -}
-- |Using @view pattern@.
--
-- >>> putQueue (hello :: TwoList Char)
-- hello
--
putQueue (dequeue -> Nothing) = return ()
putQueue (dequeue -> Just (x, xs)) = putChar x >> putQueue xs

-- |Using @pattern guard@.
--
-- >>> putQueue' (hello :: TwoList Char)
-- hello
--
putQueue' q
	| Nothing <- dequeue q = return ()
	| Just (x, xs) <- dequeue q = putChar x >> putQueue xs
