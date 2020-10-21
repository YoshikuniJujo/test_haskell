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

newGdkWindowAttr :: GdkWindowAttr' -> IO (ForeignPtr GdkWindowAttr, #{type GdkWindowAttributesType})
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

newForeignCString :: String -> IO ForeignCString
newForeignCString s = do
	cs <- newCString s
	newForeignPtr cs (free cs)

type ForeignCString = ForeignPtr CChar
