{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.Windows.GdkWindowAttr (
	GdkWindowAttr(..), newGdkWindowAttr, mkGdkWindowAttr,
	GdkWindowType(..),
	pattern GdkWindowRoot, pattern GdkWindowToplevel,
	pattern GdkWindowChild, pattern GdkWindowTemp,
	pattern GdkWindowForeign, pattern GdkWindowOffscreen,
	pattern GdkWindowSubsurface ) where

import Foreign.ForeignPtr hiding (newForeignPtr, addForeignPtrFinalizer)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String
import Foreign.C.Enum
import Data.Bits
import Data.Bool
import Data.Maybe
import Data.Word
import Data.Int

import Graphics.Gdk.Visuals
import Graphics.Gdk.Cursors
import Graphics.Gdk.Values

#include <gdk/gdk.h>

enum "GdkWindowType" ''#{type GdkWindowType} [''Show] [
	("GdkWindowRoot", #{const GDK_WINDOW_ROOT}),
	("GdkWindowToplevel", #{const GDK_WINDOW_TOPLEVEL}),
	("GdkWindowChild", #{const GDK_WINDOW_CHILD}),
	("GdkWindowTemp", #{const GDK_WINDOW_TEMP}),
	("GdkWindowForeign", #{const GDK_WINDOW_FOREIGN}),
	("GdkWindowOffscreen", #{const GDK_WINDOW_OFFSCREEN}),
	("GdkWindowSubsurface", #{const GDK_WINDOW_SUBSURFACE}) ]

newtype GdkWindowTypeHint = GdkWindowTypeHint #{type GdkWindowTypeHint} deriving Show

data GdkWindowAttr = GdkWindowAttr {
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

mkGdkWindowAttr ::
	[GdkEventMask] -> #{type gint} -> #{type gint} ->
	GdkWindowWindowClass -> GdkWindowType -> GdkWindowAttr
mkGdkWindowAttr em w h wc wt = GdkWindowAttr
	Nothing em Nothing Nothing w h wc Nothing wt Nothing Nothing Nothing

newGdkWindowAttr :: GdkWindowAttr -> IO (ForeignPtr GdkWindowAttr, #{type GdkWindowAttributesType})
newGdkWindowAttr wattr = do
	p <- mallocBytes #{size GdkWindowAttr}
	fp <- newForeignPtr p (free p)
	fpoke fp wattr
	pure (fp, gdkWindowAttributesTypeMerged wattr)
	where
	fpoke fa a = withForeignPtr fa \pa -> do
		case gdkWindowAttrTitle a of
			Nothing -> pure ()
			Just ttl -> do
				ttl' <- newForeignCString ttl
				withForeignPtr ttl' \ttl'' ->
					#{poke GdkWindowAttr, title} pa ttl''
				addForeignPtrFinalizer fa $ touchForeignPtr ttl'
		#{poke GdkWindowAttr, event_mask} pa
			. mergeGdkEventMask $ gdkWindowAttrEventMask a
		maybe (pure ()) (#{poke GdkWindowAttr, x} pa) $ gdkWindowAttrX a
		maybe (pure ()) (#{poke GdkWindowAttr, y} pa) $ gdkWindowAttrY a
		#{poke GdkWindowAttr, width} pa $ gdkWindowAttrWidth a
		#{poke GdkWindowAttr, height} pa $ gdkWindowAttrHeight a
		#{poke GdkWindowAttr, wclass} pa
			. (\(GdkWindowWindowClass c) -> c) $ gdkWindowAttrWclass a
		maybe (pure ())
			(#{poke GdkWindowAttr, visual} pa . (\(GdkVisual v) -> v))
			(gdkWindowAttrVisual a)
		#{poke GdkWindowAttr, window_type} pa
			. (\(GdkWindowType t) -> t) $ gdkWindowAttrWindowType a
		case gdkWindowAttrCursor a of
			Nothing -> pure ()
			Just (GdkCursor fc) -> withForeignPtr fc \pc ->
				#{poke GdkWindowAttr, cursor} pa pc
		maybe (pure ())
			(#{poke GdkWindowAttr, override_redirect} pa)
			(gdkWindowAttrOverrideRedirect a)
		maybe (pure ())
			(#{poke GdkWindowAttr, type_hint} pa . (\(GdkWindowTypeHint th) -> th))
			(gdkWindowAttrTypeHint a)

gdkWindowAttributesTypeMerged :: GdkWindowAttr -> #type GdkWindowAttributesType
gdkWindowAttributesTypeMerged = merge . gdkWindowAttributesTypeList
	where merge [] = 0; merge (at : ats) = at .|. merge ats

gdkWindowAttributesTypeList :: GdkWindowAttr -> [#type GdkWindowAttributesType]
gdkWindowAttributesTypeList a = catMaybes [
	bool Nothing (Just #const GDK_WA_TITLE) . isJust $ gdkWindowAttrTitle a,
	bool Nothing (Just #const GDK_WA_X) . isJust $ gdkWindowAttrX a,
	bool Nothing (Just #const GDK_WA_Y) . isJust $ gdkWindowAttrY a,
	bool Nothing (Just #const GDK_WA_CURSOR) . isJust $ gdkWindowAttrCursor a,
	bool Nothing (Just #const GDK_WA_VISUAL) . isJust $ gdkWindowAttrVisual a,
	bool Nothing (Just #const GDK_WA_NOREDIR) . isJust $ gdkWindowAttrOverrideRedirect a,
	bool Nothing (Just #const GDK_WA_TYPE_HINT) . isJust $ gdkWindowAttrTypeHint a ]

type ForeignCString = ForeignPtr CChar

newForeignCString :: String -> IO ForeignCString
newForeignCString s = do
	cs <- newCString s
	newForeignPtr cs (free cs)

enum "GdkWindowAttributesType" ''#{type GdkWindowAttributesType} [''Show] [
	("GdkWaTitle", #{const GDK_WA_TITLE}),
	("GdkWaX", #{const GDK_WA_X}), ("GdkWaY", #{const GDK_WA_Y}),
	("GdkWaCursor", #{const GDK_WA_CURSOR}),
	("GdkWaVisual", #{const GDK_WA_VISUAL}),
	("GdkWaWmclass", #{const GDK_WA_WMCLASS}),
	("GdkWaTypeHint", #{const GDK_WA_TYPE_HINT}) ]
