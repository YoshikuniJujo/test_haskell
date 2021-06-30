{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.Windows.GdkWindowAttr (
	GdkWindowAttr(..), withGdkWindowAttr, minimalGdkWindowAttr,
	GdkWindowAttributesTypes(..),
	GdkWindowType(..),
	pattern GdkWindowRoot, pattern GdkWindowToplevel,
	pattern GdkWindowChild, pattern GdkWindowTemp,
	pattern GdkWindowForeign, pattern GdkWindowOffscreen,
	pattern GdkWindowSubsurface ) where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr, addForeignPtrFinalizer)
import Foreign.Marshal hiding (void)
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String
import Foreign.C.Enum
import Control.Monad
import Data.Bits
import Data.Bool
import Data.Maybe
import Data.Word

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

newtype GdkWindowAttributesTypes =
	GdkWindowAttributesTypes #{type GdkWindowAttributesType} deriving Show

emptyGdkWindowAttributesTypes :: GdkWindowAttributesTypes
emptyGdkWindowAttributesTypes = GdkWindowAttributesTypes 0

enum "GdkWindowAttributesType" ''#{type GdkWindowAttributesType} [''Show] [
	("GdkWaTitle", #{const GDK_WA_TITLE}),
	("GdkWaX", #{const GDK_WA_X}), ("GdkWaY", #{const GDK_WA_Y}),
	("GdkWaCursor", #{const GDK_WA_CURSOR}),
	("GdkWaVisual", #{const GDK_WA_VISUAL}),
	("GdkWaWmclass", #{const GDK_WA_WMCLASS}),
	("GdkWaNoredir", #{const GDK_WA_NOREDIR}),
	("GdkWaTypeHint", #{const GDK_WA_TYPE_HINT}) ]

data GdkWindowAttr = GdkWindowAttr {
	gdkWindowAttrTitle :: Maybe String,
	gdkWindowAttrEventMask :: [GdkEventMask],
	gdkWindowAttrX, gdkWindowAttrY :: Maybe CInt,
	gdkWindowAttrWidth, gdkWindowAttrHeight :: CInt,
	gdkWindowAttrWclass :: GdkWindowWindowClass,
	gdkWindowAttrVisual :: Maybe GdkVisual,
	gdkWindowAttrWindowType :: GdkWindowType,
	gdkWindowAttrCursor :: Maybe GdkCursor,
	gdkWindowAttrOverrideRedirect :: Maybe Bool,
	gdkWindowAttrTypeHint :: Maybe GdkWindowTypeHint } deriving Show

gdkWindowAttrWclassRaw :: GdkWindowAttr -> #{type GdkWindowWindowClass}
gdkWindowAttrWclassRaw = (\(GdkWindowWindowClass c) -> c) . gdkWindowAttrWclass

gdkWindowAttrVisualPtr :: GdkWindowAttr -> Maybe (Ptr GdkVisual)
gdkWindowAttrVisualPtr a = (\(GdkVisual v) -> v) <$> gdkWindowAttrVisual a

gdkWindowAttrWindowTypeRaw :: GdkWindowAttr -> #{type GdkWindowType}
gdkWindowAttrWindowTypeRaw = (\(GdkWindowType t) -> t) . gdkWindowAttrWindowType

minimalGdkWindowAttr ::
	[GdkEventMask] -> CInt -> CInt ->
	GdkWindowWindowClass -> GdkWindowType -> GdkWindowAttr
minimalGdkWindowAttr em w h wc wt = GdkWindowAttr
	Nothing em Nothing Nothing w h wc Nothing wt Nothing Nothing Nothing

withGdkWindowAttr :: GdkWindowAttr ->
	(Ptr GdkWindowAttr -> GdkWindowAttributesTypes -> IO a) -> IO a
withGdkWindowAttr attr f =
	allocaBytes #{size GdkWindowAttr} \pattr -> case gdkWindowAttrTitle attr of
		Nothing -> do
			setAttributes pattr attr
			f pattr $ gdkWindowAttrToTypes attr
		Just ttl -> withCString ttl \cttl -> do
			#{poke GdkWindowAttr, title} pattr cttl
			setAttributes pattr attr
			f pattr $ gdkWindowAttrToTypes attr

setAttributes :: Ptr b -> GdkWindowAttr -> IO ()
setAttributes pa a = do
		#{poke GdkWindowAttr, event_mask} pa
			. mergeGdkEventMask $ gdkWindowAttrEventMask a
		whenMaybe (#{poke GdkWindowAttr, x} pa) $ gdkWindowAttrX a
		whenMaybe (#{poke GdkWindowAttr, y} pa) $ gdkWindowAttrY a
		#{poke GdkWindowAttr, width} pa $ gdkWindowAttrWidth a
		#{poke GdkWindowAttr, height} pa $ gdkWindowAttrHeight a
		#{poke GdkWindowAttr, wclass} pa $ gdkWindowAttrWclassRaw a
		whenMaybe (#{poke GdkWindowAttr, visual} pa)
			$ gdkWindowAttrVisualPtr a
		#{poke GdkWindowAttr, window_type} pa
			$ gdkWindowAttrWindowTypeRaw a
		case gdkWindowAttrCursor a of
			Nothing -> pure ()
			Just (GdkCursor fc) -> withForeignPtr fc \pc ->
				#{poke GdkWindowAttr, cursor} pa pc
		whenMaybe
			(#{poke GdkWindowAttr, override_redirect} pa)
			(gdkWindowAttrOverrideRedirect a)
		whenMaybe
			(#{poke GdkWindowAttr, type_hint} pa . (\(GdkWindowTypeHint th) -> th))
			(gdkWindowAttrTypeHint a)

whenMaybe :: Applicative m => (a -> m b) -> Maybe a -> m ()
whenMaybe f mx = maybe (pure ()) (void . f) mx

gdkWindowAttrToTypes :: GdkWindowAttr -> GdkWindowAttributesTypes
gdkWindowAttrToTypes = gdkWindowAttributesTypes . gdkWindowAttrToTypeList

gdkWindowAttrToTypeList :: GdkWindowAttr -> [GdkWindowAttributesType]
gdkWindowAttrToTypeList a = catMaybes [
	bool Nothing (Just GdkWaTitle) . isJust $ gdkWindowAttrTitle a,
	bool Nothing (Just GdkWaX) . isJust $ gdkWindowAttrX a,
	bool Nothing (Just GdkWaY) . isJust $ gdkWindowAttrY a,
	bool Nothing (Just GdkWaCursor) . isJust $ gdkWindowAttrCursor a,
	bool Nothing (Just GdkWaVisual) . isJust $ gdkWindowAttrVisual a,
	bool Nothing (Just GdkWaNoredir)
		. isJust $ gdkWindowAttrOverrideRedirect a,
	bool Nothing (Just GdkWaTypeHint) . isJust $ gdkWindowAttrTypeHint a ]

consGdkWindowAttributesType ::
	GdkWindowAttributesType -> GdkWindowAttributesTypes ->
	GdkWindowAttributesTypes
consGdkWindowAttributesType
	(GdkWindowAttributesType t) (GdkWindowAttributesTypes ts) =
	GdkWindowAttributesTypes $ t .|. ts

gdkWindowAttributesTypes ::
	[GdkWindowAttributesType] -> GdkWindowAttributesTypes
gdkWindowAttributesTypes =
	foldr consGdkWindowAttributesType emptyGdkWindowAttributesTypes
