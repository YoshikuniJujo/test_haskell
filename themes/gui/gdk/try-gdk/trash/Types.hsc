{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.Types where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr, addForeignPtrFinalizer)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Foreign.C
import Data.Bits
import Data.Bool
import Data.Maybe
import Data.Word
import Data.Int

import Graphics.Gdk.Values

#include <gdk/gdk.h>
#include "try-gdk.h"

newtype GdkWindow = GdkWindow (ForeignPtr GdkWindow) deriving Show
newtype GdkWindowAttr = GdkWindowAttr (Ptr GdkWindowAttr) deriving Show

gdkWindowAttrSetEventMask :: GdkWindowAttr -> [GdkEventMask] -> IO ()
gdkWindowAttrSetEventMask (GdkWindowAttr attr) ems =
	#{poke GdkWindowAttr, event_mask} attr $ mergeGdkEventMask ems

gdkWindowAttrSetWindowType :: GdkWindowAttr -> GdkWindowType -> IO ()
gdkWindowAttrSetWindowType (GdkWindowAttr attr) (GdkWindowType wt) = #{poke GdkWindowAttr, window_type} attr wt

gdkWindowAttrSetX, gdkWindowAttrSetY :: GdkWindowAttr -> #{type gint} -> IO ()
gdkWindowAttrSetX (GdkWindowAttr attr) = #{poke GdkWindowAttr, x} attr
gdkWindowAttrSetY (GdkWindowAttr attr) = #{poke GdkWindowAttr, y} attr

gdkWindowAttrSetWidth, gdkWindowAttrSetHeight :: GdkWindowAttr -> #{type gint} -> IO ()
gdkWindowAttrSetWidth (GdkWindowAttr attr) = #{poke GdkWindowAttr, width} attr
gdkWindowAttrSetHeight (GdkWindowAttr attr) = #{poke GdkWindowAttr, height} attr

gdkWindowAttrSetWClass :: GdkWindowAttr -> GdkWindowWindowClass -> IO ()
gdkWindowAttrSetWClass (GdkWindowAttr attr) (GdkWindowWindowClass c) = #{poke GdkWindowAttr, wclass} attr c

instance Storable GdkWindowAttr where
	sizeOf _ = #{size GdkWindowAttr}
	alignment _ = #{alignment GdkWindowAttr}
	peek = pure . GdkWindowAttr
	poke _ _ = pure ()

foreign import ccall "gdk_window_destroy" c_gdk_window_destroy ::
	Ptr GdkWindow -> IO ()

makeGdkWindow :: Ptr GdkWindow -> IO GdkWindow
makeGdkWindow p = GdkWindow <$> newForeignPtr p (free p)

newtype GdkDrawingContext = GdkDrawingContext (Ptr GdkDrawingContext) deriving Show

newtype GdkRectangle = GdkRectangle (Ptr GdkRectangle) deriving Show

data GdkWindowAttr' = GdkWindowAttr' {
	gdkWindowAttrTitle :: Maybe String,
	gdkWindowAttrEventMask :: [GdkEventMask],
	gdkWindowAttrX, gdkWindowAttrY :: Maybe #{type gint},
	gdkWindowAttrWidth, gdkWindowAttrHeight :: #{type gint},
	gdkWindowAttrWclass :: GdkWindowWindowClass,
	gdkWindowAttrVisual :: Maybe GdkVisual,
	gdkWindowAttrWindowType :: GdkWindowType,
	gdkWindowAttrCursor :: Maybe GdkCursor,
	gdkWindowAttrOverrideRedirect :: Maybe Bool,
	gdkWindowAttrTypeHint :: Maybe GdkWindowTypeHint } deriving Show

newtype GdkVisual = GdkVisual (Ptr GdkVisual) deriving Show
newtype GdkCursor = GdkCursor (ForeignPtr GdkCursor) deriving Show

newtype GdkWindowTypeHint = GdkWindowTypeHint #{type GdkWindowTypeHint} deriving Show

instance Storable GdkWindowAttr' where
	sizeOf _ = #{size GdkWindowAttrWindowAttributesType}
	alignment _ = #{alignment GdkWindowAttrWindowAttributesType}
	peek p = GdkWindowAttr'
		<$> undefined
		<*> undefined
		<*> undefined
		<*> undefined
		<*> undefined
		<*> undefined
		<*> undefined
		<*> undefined
		<*> undefined
		<*> undefined
		<*> undefined
		<*> undefined
	poke p a = do
--		ttl <- newForeignCString 
--		maybe (pure ())
--			(#{poke GdkWindowAttrWindowAttributesType, gdk_window_attr.title} p) 
--			(gdkWindowAttrTitle a)
		maybe (pure ())
			(#{poke GdkWindowAttrWindowAttributesType, gdk_window_attr.x} p)
			(gdkWindowAttrX a)

type ForeignCString = ForeignPtr CChar

newForeignCString :: String -> IO ForeignCString
newForeignCString s = do
	cs <- newCString s
	newForeignPtr cs (free cs)

gdkWindowAttributesTypeMerged :: GdkWindowAttr' -> #type GdkWindowAttributesType
gdkWindowAttributesTypeMerged = merge . gdkWindowAttributesTypeList
	where merge [] = 0; merge (at : ats) = at .|. merge ats

gdkWindowAttributesTypeList :: GdkWindowAttr' -> [#type GdkWindowAttributesType]
gdkWindowAttributesTypeList a = catMaybes [
	bool Nothing (Just #const GDK_WA_TITLE) . isJust $ gdkWindowAttrTitle a,
	bool Nothing (Just #const GDK_WA_X) . isJust $ gdkWindowAttrX a,
	bool Nothing (Just #const GDK_WA_Y) . isJust $ gdkWindowAttrY a,
	bool Nothing (Just #const GDK_WA_CURSOR) . isJust $ gdkWindowAttrCursor a,
	bool Nothing (Just #const GDK_WA_VISUAL) . isJust $ gdkWindowAttrVisual a,
	bool Nothing (Just #const GDK_WA_NOREDIR) . isJust $ gdkWindowAttrOverrideRedirect a,
	bool Nothing (Just #const GDK_WA_TYPE_HINT) . isJust $ gdkWindowAttrTypeHint a ]

foo :: Ptr a -> IO #type gint
foo = #peek GdkWindowAttrWindowAttributesType, gdk_window_attr.x

class ForeignStorable a where
	fsize :: Int
	falignment :: Int
	fpeek :: ForeignPtr a -> IO a
	fpoke :: ForeignPtr a -> a -> IO ()

instance ForeignStorable GdkWindowAttr' where
	fsize = #size GdkWindowAttrWindowAttributesType
	falignment = #alignment GdkWindowAttrWindowAttributesType
--	fpeek fa = withForeignPtr fa \pa -> 
	fpoke fa a = withForeignPtr fa \pa -> do
		case gdkWindowAttrTitle a of
			Nothing -> pure ()
			Just ttl -> do
				ttl' <- newForeignCString ttl
				withForeignPtr ttl' \ttl'' ->
					#{poke GdkWindowAttrWindowAttributesType, gdk_window_attr.title} pa ttl''
				addForeignPtrFinalizer fa $ touchForeignPtr ttl'
		#{poke GdkWindowAttrWindowAttributesType, gdk_window_attr.event_mask} pa
			. mergeGdkEventMask $ gdkWindowAttrEventMask a
		maybe (pure ()) (#{poke GdkWindowAttrWindowAttributesType, gdk_window_attr.x} pa) $ gdkWindowAttrX a
		maybe (pure ()) (#{poke GdkWindowAttrWindowAttributesType, gdk_window_attr.y} pa) $ gdkWindowAttrY a
		#{poke GdkWindowAttrWindowAttributesType, gdk_window_attr.width} pa $ gdkWindowAttrWidth a
		#{poke GdkWindowAttrWindowAttributesType, gdk_window_attr.height} pa $ gdkWindowAttrHeight a
		#{poke GdkWindowAttrWindowAttributesType, gdk_window_attr.wclass} pa
			. (\(GdkWindowWindowClass c) -> c) $ gdkWindowAttrWclass a
		maybe (pure ())
			(#{poke GdkWindowAttrWindowAttributesType, gdk_window_attr.visual} pa . (\(GdkVisual v) -> v))
			(gdkWindowAttrVisual a)
		#{poke GdkWindowAttrWindowAttributesType, gdk_window_attr.window_type} pa
			. (\(GdkWindowType t) -> t) $ gdkWindowAttrWindowType a
		case gdkWindowAttrCursor a of
			Nothing -> pure ()
			Just (GdkCursor fc) -> withForeignPtr fc \pc ->
				#{poke GdkWindowAttrWindowAttributesType, gdk_window_attr.cursor} pa pc
		maybe (pure ())
			(#{poke GdkWindowAttrWindowAttributesType, gdk_window_attr.override_redirect} pa)
			(gdkWindowAttrOverrideRedirect a)
		maybe (pure ())
			(#{poke GdkWindowAttrWindowAttributesType, gdk_window_attr.type_hint} pa . (\(GdkWindowTypeHint th) -> th))
			(gdkWindowAttrTypeHint a)
		#{poke GdkWindowAttrWindowAttributesType, gdk_window_attributes_type} pa
			$ gdkWindowAttributesTypeMerged a
		
boolToGboolean :: Bool -> #type gboolean
boolToGboolean = \case False -> #{const FALSE}; True -> #{const TRUE}

data GdkWindowAttributesTypeAll = GdkWindowAttributesTypeAll {
	gdkWindowAttributesTypeTitle :: Bool,
	gdkWindowAttributesTypeX, gdkWindowAttributesTypeY :: Bool,
	gdkWindowAttributesTypeCursor :: Bool,
	gdkWindowAttributesTypeVisual :: Bool,
	gdkWindowAttributesTypeNoredir :: Bool,
	gdkWindowAttributesTypeTypeHint :: Bool }

scrapGdkWindowAttributesType :: GdkWindowAttributesTypeAll -> [GdkWindowAttributesType]
scrapGdkWindowAttributesType at = catMaybes [
	bool Nothing (Just gdkWaTitle) $ gdkWindowAttributesTypeTitle at,
	bool Nothing (Just gdkWaX) $ gdkWindowAttributesTypeX at,
	bool Nothing (Just gdkWaY) $ gdkWindowAttributesTypeY at,
	bool Nothing (Just gdkWaCursor) $ gdkWindowAttributesTypeCursor at,
	bool Nothing (Just gdkWaVisual) $ gdkWindowAttributesTypeVisual at,
	bool Nothing (Just gdkWaNoredir) $ gdkWindowAttributesTypeNoredir at,
	bool Nothing (Just gdkWaTypeHint) $ gdkWindowAttributesTypeTypeHint at ]

readGdkWindowAttributesTypeAll :: #{type GdkWindowAttributesType} -> GdkWindowAttributesTypeAll
readGdkWindowAttributesTypeAll at = GdkWindowAttributesTypeAll {
	gdkWindowAttributesTypeTitle = at .&. #{const GDK_WA_TITLE} /= 0,
	gdkWindowAttributesTypeX = at .&. #{const GDK_WA_X} /= 0,
	gdkWindowAttributesTypeY = at .&. #{const GDK_WA_Y} /= 0,
	gdkWindowAttributesTypeCursor = at .&. #{const GDK_WA_CURSOR} /= 0,
	gdkWindowAttributesTypeVisual = at .&. #{const GDK_WA_VISUAL} /= 0,
	gdkWindowAttributesTypeNoredir = at .&. #{const GDK_WA_NOREDIR} /= 0,
	gdkWindowAttributesTypeTypeHint = at .&. #{const GDK_WA_TYPE_HINT} /= 0 }

instance Storable GdkWindowAttributesTypeAll where
	sizeOf _ = #size GdkWindowAttributesType
	alignment _ = #alignment GdkWindowAttributesType
	peek p = readGdkWindowAttributesTypeAll <$> peek (castPtr p)
	poke p = poke (castPtr p) . mergeGdkWindowAttributesType . scrapGdkWindowAttributesType

peekGdkWindowAttr :: GdkWindowAttributesTypeAll -> Ptr GdkWindowAttr' -> IO GdkWindowAttr'
peekGdkWindowAttr ata p = GdkWindowAttr'
	<$> bool (pure Nothing) (Just <$> #{peek GdkWindowAttrWindowAttributesType, gdk_window_attr.title} p)
		(gdkWindowAttributesTypeTitle ata)
	<*> (unmergeGdkEventMask <$> #{peek GdkWindowAttrWindowAttributesType, gdk_window_attr.event_mask} p)
	<*> bool (pure Nothing) (Just <$> #{peek GdkWindowAttrWindowAttributesType, gdk_window_attr.x} p)
		(gdkWindowAttributesTypeX ata)
	<*> bool (pure Nothing) (Just <$> #{peek GdkWindowAttrWindowAttributesType, gdk_window_attr.y} p)
		(gdkWindowAttributesTypeY ata)
	<*> #{peek GdkWindowAttrWindowAttributesType, gdk_window_attr.width} p
	<*> #{peek GdkWindowAttrWindowAttributesType, gdk_window_attr.height} p
	<*> (GdkWindowWindowClass <$> #{peek GdkWindowAttrWindowAttributesType, gdk_window_attr.wclass} p)
	<*> bool (pure Nothing) (Just <$> #{peek GdkWindowAttrWindowAttributesType, gdk_window_attr.visual} p)
		(gdkWindowAttributesTypeVisual ata)
	<*> (GdkWindowType <$> #{peek GdkWindowAttrWindowAttributesType, gdk_window_attr.window_type} p)
	<*> bool (pure Nothing) (Just . GdkCursor <$> #{peek GdkWindowAttrWindowAttributesType, gdk_window_attr.cursor} p)
		(gdkWindowAttributesTypeCursor ata)
	<*> bool (pure Nothing) (Just <$> #{peek GdkWindowAttrWindowAttributesType, gdk_window_attr.override_redirect} p)
		(gdkWindowAttributesTypeNoredir ata)
	<*> bool (pure Nothing) (Just . GdkWindowTypeHint <$> #{peek GdkWindowAttrWindowAttributesType, gdk_window_attr.type_hint} p)
		(gdkWindowAttributesTypeTypeHint ata)
