{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module System.Glib.DoublyLinkedLists where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable

#include <glib.h>

newtype GList a = GList (ForeignPtr (GList a)) deriving Show
newtype GListRef a = GListRef (Ptr (GList a)) deriving Show

g_list_prd_dat_nxt :: Ptr (GList a) -> IO (Ptr (GList a), Ptr a, Ptr (GList a))
g_list_prd_dat_nxt p = (,,) <$> #{peek GList, prev} p <*> #{peek GList, data} p <*> #{peek GList, next} p

gListData :: Storable a => GListRef a -> IO (Maybe a)
gListData (GListRef lst)
	| lst == nullPtr = pure Nothing
	| otherwise = do
		(_, p, _) <- g_list_prd_dat_nxt lst
		Just <$> peek p

gListDataPtr :: GListRef a -> IO (Maybe (Ptr a))
gListDataPtr (GListRef lst)
	| lst == nullPtr = pure Nothing
	| otherwise = do
		(_, p, _) <- g_list_prd_dat_nxt lst
		pure $ Just p

gListPred :: GListRef a -> IO (Maybe (GListRef a))
gListPred (GListRef lst)
	| lst == nullPtr = pure Nothing
	| otherwise = do
		(prd, _, _) <- g_list_prd_dat_nxt lst
		pure . Just $ GListRef prd

gListNext :: GListRef a -> IO (Maybe (GListRef a))
gListNext (GListRef lst)
	| lst == nullPtr = pure Nothing
	| otherwise = do
		(_, _, nxt) <- g_list_prd_dat_nxt lst
		pure . Just $ GListRef nxt

gListPredListPtr :: GListRef a -> IO [Ptr a]
gListPredListPtr lst = ((,) <$> gListDataPtr lst <*> gListPred lst) >>= \case
	(Nothing, Nothing) -> pure []
	(Just p, Just prd) -> (p :) <$> gListPredListPtr prd
	_ -> error "never occur"

gListNextListPtr :: GListRef a -> IO [Ptr a]
gListNextListPtr lst = ((,) <$> gListDataPtr lst <*> gListNext lst) >>= \case
	(Nothing, Nothing) -> pure []
	(Just p, Just prd) -> (p :) <$> gListNextListPtr prd
	_ -> error "never occur"

gListListPtr :: GListRef a -> IO ([Ptr a], [Ptr a])
gListListPtr lst = gListPred lst >>= \case
	Nothing -> pure ([], [])
	Just prd -> (,) <$> gListPredListPtr prd <*> gListNextListPtr lst
