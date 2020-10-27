{-# LANGUAGE BlockArguments #-}
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

newtype GdkWindow = GdkWindow (Ptr GdkWindow) deriving Show

newtype GdkDrawingContext = GdkDrawingContext (Ptr GdkDrawingContext) deriving Show

-- newtype GdkRectangle = GdkRectangle (Ptr GdkRectangle) deriving Show

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

newtype GdkVisual = GdkVisual (Ptr GdkVisual) deriving Show
newtype GdkCursor = GdkCursor (ForeignPtr GdkCursor) deriving Show
newtype GdkCursorRef = GdkCursorRef (Ptr GdkCursor) deriving Show

mkGdkCursor :: Ptr GdkCursor -> IO GdkCursor
mkGdkCursor p = GdkCursor <$> newForeignPtr p (c_g_object_unref p)

foreign import ccall "g_object_unref" c_g_object_unref :: Ptr a -> IO ()

newtype GdkWindowTypeHint = GdkWindowTypeHint #{type GdkWindowTypeHint} deriving Show

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

newForeignCString :: String -> IO ForeignCString
newForeignCString s = do
	cs <- newCString s
	newForeignPtr cs (free cs)

type ForeignCString = ForeignPtr CChar

newtype GdkDisplay = GdkDisplay (Ptr GdkDisplay) deriving Show

newtype GdkScreen = GdkScreen (Ptr GdkScreen) deriving Show

newtype GdkDisplayManager = GdkDisplayManager (Ptr GdkDisplayManager) deriving Show

newtype GdkSeat = GdkSeat (Ptr GdkSeat) deriving Show

newtype GdkDevice = GdkDevice (Ptr GdkDevice) deriving Show

newtype GdkMonitor = GdkMonitor (Ptr GdkMonitor) deriving Show

data GdkRectangle = GdkRectangle {
	gdkRectangleX, gdkRectangleY :: #{type int},
	gdkRectangleWidth, gdkRectangleHeight :: #{type int}
	} deriving Show

instance Storable GdkRectangle where
	sizeOf _ = #{size GdkRectangle}
	alignment _ = #{alignment GdkRectangle}
	peek p = GdkRectangle
		<$> #{peek GdkRectangle, x} p <*> #{peek GdkRectangle, y} p
		<*> #{peek GdkRectangle, width} p <*> #{peek GdkRectangle, height} p
	poke p gp = do
		#{poke GdkRectangle, x} p $ gdkRectangleX gp
		#{poke GdkRectangle, y} p $ gdkRectangleY gp
		#{poke GdkRectangle, width} p $ gdkRectangleWidth gp
		#{poke GdkRectangle, height} p $ gdkRectangleHeight gp
