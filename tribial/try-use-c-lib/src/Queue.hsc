{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Queue where

import Prelude hiding (null)

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr, addForeignPtrFinalizer)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Data.Foldable hiding (null, toList)
import Data.Word

#include <stdbool.h>

newtype Queue a = Queue (ForeignPtr (Queue a)) deriving Show

queueNew :: IO (Queue a)
queueNew = Queue <$> do
	pq <- c_queue_new
	newForeignPtr pq $ c_queue_destroy pq

foreign import ccall "queue_new" c_queue_new :: IO (Ptr (Queue a))
foreign import ccall "queue_destroy" c_queue_destroy :: Ptr (Queue a) -> IO ()

null :: Queue a -> IO Bool
null (Queue fq) = withForeignPtr fq \pq -> do
	b <- c_null pq
	pure case b of
		#{const false} -> False
		_ -> True

foreign import ccall "null" c_null :: Ptr (Queue a) -> IO #{type bool}

cons :: Storable a => a -> Queue a -> IO ()
cons x (Queue fq) = do
	p <- malloc
	poke p x
	withForeignPtr fq $ c_cons p
	addForeignPtrFinalizer fq $ free p

unsnoc :: Storable a => Queue a -> IO a
unsnoc (Queue fq) = withForeignPtr fq \pq -> do
	p <- c_unsnoc pq
	peek p

foreign import ccall "cons" c_cons :: Ptr a -> Ptr (Queue a) -> IO ()
foreign import ccall "unsnoc" c_unsnoc :: Ptr (Queue a) -> IO (Ptr a)

fromList :: Storable a => [a] -> Queue a -> IO ()
fromList xs q = for_ xs \x -> cons x q

toList :: Storable a => Queue a -> IO [a]
toList q = do
	b <- null q
	if b then pure [] else do
		x <- unsnoc q
		(x :) <$> toList q
