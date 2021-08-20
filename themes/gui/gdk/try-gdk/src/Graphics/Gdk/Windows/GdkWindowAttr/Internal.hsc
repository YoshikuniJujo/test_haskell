{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.Windows.GdkWindowAttr.Internal (
	GdkWindowAttr(..), withGdkWindowAttr, minimalGdkWindowAttr,
	GdkWindowAttributesTypes(..),
	GdkWindowWindowClass(..), pattern GdkInputOutput, pattern GdkInputOnly,
	GdkWindowType(..),
	pattern GdkWindowRoot, pattern GdkWindowToplevel,
	pattern GdkWindowChild, pattern GdkWindowTemp,
	pattern GdkWindowForeign, pattern GdkWindowOffscreen,
	pattern GdkWindowSubsurface,
	GdkWindowTypeHint(..),
	pattern GdkWindowTypeHintNormal, pattern GdkWindowTypeHintDialog,
	pattern GdkWindowTypeHintMenu, pattern GdkWindowTypeHintToolbar,
	pattern GdkWindowTypeHintSplashscreen, pattern GdkWindowTypeHintUtility,
	pattern GdkWindowTypeHintDock, pattern GdkWindowTypeHintDesktop,
	pattern GdkWindowTypeHintDropdownMenu, pattern GdkWindowTypeHintPopupMenu,
	pattern GdkWindowTypeHintTooltip, pattern GdkWindowTypeHintNotification,
	pattern GdkWindowTypeHintCombo, pattern GdkWindowTypeHintDnd,
	whenMaybe ) where

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

import Graphics.Gdk.Visuals.Internal
import Graphics.Gdk.Cursors.Internal
import Graphics.Gdk.Windows.GdkEventMask.Internal

#include <gdk/gdk.h>

enum "GdkWindowType" ''#{type GdkWindowType} [''Show, ''Read, ''Eq] [
	("GdkWindowRoot", #{const GDK_WINDOW_ROOT}),
	("GdkWindowToplevel", #{const GDK_WINDOW_TOPLEVEL}),
	("GdkWindowChild", #{const GDK_WINDOW_CHILD}),
	("GdkWindowTemp", #{const GDK_WINDOW_TEMP}),
	("GdkWindowForeign", #{const GDK_WINDOW_FOREIGN}),
	("GdkWindowOffscreen", #{const GDK_WINDOW_OFFSCREEN}),
	("GdkWindowSubsurface", #{const GDK_WINDOW_SUBSURFACE}) ]

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

enum "GdkWindowWindowClass" ''#{type GdkWindowWindowClass} [''Show, ''Read, ''Eq] [
	("GdkInputOutput", #{const GDK_INPUT_OUTPUT}),
	("GdkInputOnly", #{const GDK_INPUT_ONLY}) ]

enum "GdkWindowTypeHint" ''#{type GdkWindowTypeHint} [''Show, ''Read, ''Eq] [
	("GdkWindowTypeHintNormal", #{const GDK_WINDOW_TYPE_HINT_NORMAL}),
	("GdkWindowTypeHintDialog", #{const GDK_WINDOW_TYPE_HINT_DIALOG}),
	("GdkWindowTypeHintMenu", #{const GDK_WINDOW_TYPE_HINT_MENU}),
	("GdkWindowTypeHintToolbar", #{const GDK_WINDOW_TYPE_HINT_TOOLBAR}),
	("GdkWindowTypeHintSplashscreen",
		#{const GDK_WINDOW_TYPE_HINT_SPLASHSCREEN}),
	("GdkWindowTypeHintUtility", #{const GDK_WINDOW_TYPE_HINT_UTILITY}),
	("GdkWindowTypeHintDock", #{const GDK_WINDOW_TYPE_HINT_DOCK}),
	("GdkWindowTypeHintDesktop", #{const GDK_WINDOW_TYPE_HINT_DESKTOP}),
	("GdkWindowTypeHintDropdownMenu",
		#{const GDK_WINDOW_TYPE_HINT_DROPDOWN_MENU}),
	("GdkWindowTypeHintPopupMenu",
		#{const GDK_WINDOW_TYPE_HINT_POPUP_MENU}),
	("GdkWindowTypeHintTooltip", #{const GDK_WINDOW_TYPE_HINT_TOOLTIP}),
	("GdkWindowTypeHintNotification",
		#{const GDK_WINDOW_TYPE_HINT_NOTIFICATION}),
	("GdkWindowTypeHintCombo", #{const GDK_WINDOW_TYPE_HINT_COMBO}),
	("GdkWindowTypeHintDnd", #{const GDK_WINDOW_TYPE_HINT_DND}) ]

data GdkWindowAttr = GdkWindowAttr {
	gdkWindowAttrTitle :: Maybe String,
	gdkWindowAttrEventMask :: GdkEventMaskMultiBits,
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

gdkWindowAttrTypeHintRaw :: GdkWindowAttr -> Maybe #{type GdkWindowTypeHint}
gdkWindowAttrTypeHintRaw a =
	(\(GdkWindowTypeHint th) -> th) <$> gdkWindowAttrTypeHint a

minimalGdkWindowAttr :: GdkEventMaskMultiBits -> CInt -> CInt -> GdkWindowAttr
minimalGdkWindowAttr em w h = GdkWindowAttr Nothing em Nothing Nothing w h
	GdkInputOutput Nothing GdkWindowToplevel Nothing Nothing Nothing

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
		. (\(GdkEventMaskMultiBits ms) -> ms) $ gdkWindowAttrEventMask a
	whenMaybe (#{poke GdkWindowAttr, x} pa) $ gdkWindowAttrX a
	whenMaybe (#{poke GdkWindowAttr, y} pa) $ gdkWindowAttrY a
	#{poke GdkWindowAttr, width} pa $ gdkWindowAttrWidth a
	#{poke GdkWindowAttr, height} pa $ gdkWindowAttrHeight a
	#{poke GdkWindowAttr, wclass} pa $ gdkWindowAttrWclassRaw a
	whenMaybe (#{poke GdkWindowAttr, visual} pa) $ gdkWindowAttrVisualPtr a
	#{poke GdkWindowAttr, window_type} pa $ gdkWindowAttrWindowTypeRaw a
	whenMaybe (`withGdkCursor` #{poke GdkWindowAttr, cursor} pa)
		$ gdkWindowAttrCursor a
	whenMaybe (#{poke GdkWindowAttr, override_redirect} pa)
		$ gdkWindowAttrOverrideRedirect a
	whenMaybe (#{poke GdkWindowAttr, type_hint} pa)
		$ gdkWindowAttrTypeHintRaw a

whenMaybe :: Applicative m => (a -> m b) -> Maybe a -> m ()
whenMaybe f mx = maybe (pure ()) (void . f) mx

withGdkCursor :: GdkCursor -> (Ptr GdkCursor -> IO a) -> IO a
withGdkCursor (GdkCursor fc) = withForeignPtr fc

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
