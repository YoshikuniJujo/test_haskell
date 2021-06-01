{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module System.Glib.SinglyLinkedLists (GSList, g_slist_to_list, g_slist_to_list') where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Storable

#include <glib.h>

newtype GSList a = GSList (ForeignPtr (GSList a)) deriving Show

foreign import ccall "g_slist_free" c_g_slist_free :: Ptr (GSList a) -> IO ()

makeGSList0 :: Ptr (GSList a) -> IO (GSList a)
makeGSList0 p = GSList <$> newForeignPtr p (pure ())

makeGSList :: Ptr (GSList a) -> IO (GSList a)
makeGSList p = GSList <$> newForeignPtr p (c_g_slist_free p)

g_slist_uncons :: Ptr (GSList a) -> IO (Ptr a, Ptr (GSList a))
g_slist_uncons p = (,) <$> #{peek GSList, data} p <*> #{peek GSList, next} p

g_slist_to_list, g_slist_to_list' :: Ptr (GSList a) -> IO [Ptr a]
g_slist_to_list lst
	| lst == nullPtr = pure []
	| otherwise = do
		(p, lst') <- g_slist_uncons lst
		(p :) <$> g_slist_to_list lst'

g_slist_to_list' lst = g_slist_to_list lst <* c_g_slist_free lst
