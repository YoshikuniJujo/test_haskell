{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.LowLevel.TabStops.Internal (
	-- * PANGO TAB ARRAY
	PangoTabArray(..), makePangoTabArray, pangoTabArrayThaw,
	pangoTabArrayGetTab, pangoTabArrayGetTabs,

	-- * PANGO TAB ARRAY NULLABLE
	PangoTabArrayNullable(..), makePangoTabArrayNullable,
	pangoTabArrayFromNullable, pangoTabArrayToNullable,

	-- * PANGO TAB ARRAY FIXED
	PangoTabArrayFixed, pangoTabArrayFixedFreeze,
	pangoTabArrayFixedNew, pangoTabArrayFixedSetTab,

	-- * PANGO TAB ARRAY INT
	PangoTabArrayInt, pangoTabArrayIntFreeze,
	pangoTabArrayIntNew, pangoTabArrayIntSetTab,
	) where

import Foreign.Ptr
import Foreign.Ptr.Misc
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.Enum
import Control.Arrow
import Control.Monad.Primitive
import Data.Foldable
import Data.Word
import Data.Int
import System.IO.Unsafe

import Graphics.Pango.Basic.GlyphStorage

#include <pango/pango.h>

newtype PangoTabArray = PangoTabArray (ForeignPtr PangoTabArray) deriving Show

makePangoTabArray :: Ptr PangoTabArray -> IO PangoTabArray
makePangoTabArray = \case
	NullPtr -> error "bad"
	p -> PangoTabArray <$> newForeignPtr p (c_pango_tab_array_free p)

data PangoTabArrayNullable
	= PangoTabArrayNull
	| PangoTabArrayNotNull (ForeignPtr PangoTabArray)
	deriving Show

makePangoTabArrayNullable :: Ptr PangoTabArray -> IO PangoTabArrayNullable
makePangoTabArrayNullable = \case
	NullPtr -> pure PangoTabArrayNull
	p -> PangoTabArrayNotNull <$> newForeignPtr p (c_pango_tab_array_free p)

pangoTabArrayFromNullable :: PangoTabArrayNullable -> Maybe PangoTabArray
pangoTabArrayFromNullable = \case
	PangoTabArrayNull -> Nothing
	PangoTabArrayNotNull fta -> Just $ PangoTabArray fta

pangoTabArrayToNullable :: Maybe PangoTabArray -> PangoTabArrayNullable
pangoTabArrayToNullable = \case
	Nothing -> PangoTabArrayNull
	Just (PangoTabArray fta) -> PangoTabArrayNotNull fta

newtype PangoTabArrayFixed s =
	PangoTabArrayFixed (ForeignPtr PangoTabArray) deriving Show

mkPangoTabArrayFixed ::
	Ptr PangoTabArray -> IO (PangoTabArrayFixed s)
mkPangoTabArrayFixed p =
	PangoTabArrayFixed <$> newForeignPtr p (c_pango_tab_array_free p)

newtype PangoTabArrayInt s =
	PangoTabArrayInt (ForeignPtr PangoTabArray) deriving Show

mkPangoTabArrayInt ::
	Ptr PangoTabArray -> IO (PangoTabArrayInt s)
mkPangoTabArrayInt p =
	PangoTabArrayInt <$> newForeignPtr p (c_pango_tab_array_free p)

foreign import ccall "pango_tab_array_free" c_pango_tab_array_free ::
	Ptr PangoTabArray -> IO ()

pangoTabArrayFixedNew ::
	PrimMonad m => m (PangoTabArrayFixed (PrimState m))
pangoTabArrayFixedNew = unsafeIOToPrim
	$ mkPangoTabArrayFixed =<< c_pango_tab_array_new 1 #{const FALSE}

pangoTabArrayIntNew ::
	PrimMonad m => m (PangoTabArrayInt (PrimState m))
pangoTabArrayIntNew = unsafeIOToPrim
	$ mkPangoTabArrayInt =<< c_pango_tab_array_new 1 #{const TRUE}

foreign import ccall "pango_tab_array_new" c_pango_tab_array_new ::
	CInt -> #{type gboolean} -> IO (Ptr PangoTabArray)

pangoTabArrayFixedSetTab :: PrimMonad m =>
	PangoTabArrayFixed (PrimState m) -> CInt -> PangoFixed -> m ()
pangoTabArrayFixedSetTab (PangoTabArrayFixed fta) idx x = unsafeIOToPrim
	$ withForeignPtr fta \pta -> do
		sz <- c_pango_tab_array_get_size_prim pta
		if idx < sz
		then c_pango_tab_array_set_tab pta idx #{const PANGO_TAB_LEFT} $ toCInt x
		else do	lst <- tempPangoTabArrayGetTab pta (sz - 1)
			let	Just (sz', tss) = calculateFixed sz idx (fromCInt lst) x
			c_pango_tab_array_resize pta sz'
			for_ (zip [sz .. sz' - 1] tss) \(i, xx) ->
				c_pango_tab_array_set_tab pta i #{const PANGO_TAB_LEFT}
					. round $ xx * #{const PANGO_SCALE}

foreign import ccall "pango_tab_array_set_tab" c_pango_tab_array_set_tab ::
	Ptr PangoTabArray -> CInt -> #{type PangoTabAlign} -> CInt -> IO ()

tempPangoTabArrayGetTab :: Ptr PangoTabArray -> CInt -> IO CInt
tempPangoTabArrayGetTab pta idx = alloca \px -> do
	c_pango_tab_array_get_tab_prim pta idx nullPtr px
	peek px

foreign import ccall "pango_tab_array_get_tab" c_pango_tab_array_get_tab_prim ::
	Ptr PangoTabArray -> CInt -> Ptr #{type PangoTabAlign} -> Ptr CInt -> IO ()

calculateFixed :: CInt -> CInt -> PangoFixed -> PangoFixed -> Maybe (CInt, [PangoFixed])
calculateFixed sz idx lst x
	| idx < sz = Nothing
	| otherwise = Just (sz', take (fromIntegral $ sz' - sz) [lst + dx, lst + 2 * dx ..])
	where
	sz' = nextPow2 $ idx + 1
	dx = (x - lst) / fromIntegral (idx - sz + 1)

calculateDouble :: CInt -> CInt -> Double -> Double -> Maybe (CInt, [Double])
calculateDouble sz idx lst x
	| idx < sz = Nothing
	| otherwise = Just (sz', take (fromIntegral $ sz' - sz) [lst + dx, lst + 2 * dx ..])
	where
	sz' = nextPow2 $ idx + 1
	dx = (x - lst) / fromIntegral (idx - sz + 1)

nextPow2 :: (Ord n, Num n) => n -> n
nextPow2 k = head . dropWhile (< k) $ (2 ^) <$> [0 :: Int ..]

pangoTabArrayIntSetTab :: PrimMonad m =>
	PangoTabArrayInt (PrimState m) -> CInt -> CInt -> m ()
pangoTabArrayIntSetTab (PangoTabArrayInt fta) idx x = unsafeIOToPrim
	$ withForeignPtr fta \pta -> do
		sz <- c_pango_tab_array_get_size_prim pta
		if idx < sz
		then c_pango_tab_array_set_tab pta idx #{const PANGO_TAB_LEFT} x
		else do	lst <- tempPangoTabArrayGetTab pta (sz - 1)
			let	Just (sz', tss) = calculateCInt sz idx lst x
			c_pango_tab_array_resize pta sz'
			for_ (zip [sz .. sz' - 1] tss) \(i, xx) ->
				c_pango_tab_array_set_tab pta i #{const PANGO_TAB_LEFT} xx

calculateCInt :: CInt -> CInt -> CInt -> CInt -> Maybe (CInt, [CInt])
calculateCInt sz idx lst x =
	second (round <$>) <$> calculateDouble sz idx (fromIntegral lst) (fromIntegral x)

foreign import ccall "pango_tab_array_get_size" c_pango_tab_array_get_size_prim ::
	Ptr PangoTabArray -> IO CInt

foreign import ccall "pango_tab_array_get_size" c_pango_tab_array_get_size ::
	Ptr PangoTabArray -> IO CInt

foreign import ccall "pango_tab_array_resize" c_pango_tab_array_resize ::
	Ptr PangoTabArray -> CInt -> IO ()

enum "PangoTabAlign" ''#{type PangoTabAlign} [''Show] [
	("PangoTabLeft", #{const PANGO_TAB_LEFT}) ]

pangoTabArrayGetTab :: PangoTabArray -> CInt -> Maybe (Either Double CInt)
pangoTabArrayGetTab (PangoTabArray fta) idx = unsafePerformIO
	$ withForeignPtr fta \pta -> alloca \loc -> do
		px <- c_pango_tab_array_get_positions_in_pixels pta
		sz <- c_pango_tab_array_get_size pta
		if 0 <= idx && idx < sz then Just <$> do
			c_pango_tab_array_get_tab pta idx nullPtr loc
			(<$> peek loc) case px of
				#{const FALSE} ->
					Left . (/ #{const PANGO_SCALE}) . fromIntegral
				#{const TRUE} -> Right
				_ -> error "never occur"
		else pure Nothing

foreign import ccall "pango_tab_array_get_tab" c_pango_tab_array_get_tab ::
	Ptr PangoTabArray -> CInt -> Ptr #{type PangoTabAlign} -> Ptr CInt -> IO ()

pangoTabArrayGetTabs :: PangoTabArray -> Either [Double] [CInt]
pangoTabArrayGetTabs (PangoTabArray fta) = unsafePerformIO
	$ withForeignPtr fta \pta -> alloca \locs -> do
		n <- c_pango_tab_array_get_size pta
		px <- c_pango_tab_array_get_positions_in_pixels pta
		c_pango_tab_array_get_tabs pta nullPtr locs
		(<$> (peekArrayAndFree n =<< peek locs)) case px of
			#{const FALSE} -> Left
				. ((/ #{const PANGO_SCALE}) . fromIntegral <$>)
			#{const TRUE} -> Right
			_ -> error "never occur"

foreign import ccall "pango_tab_array_get_tabs" c_pango_tab_array_get_tabs ::
	Ptr PangoTabArray -> Ptr (Ptr #type PangoTabAlign) -> Ptr (Ptr CInt) -> IO ()

peekArrayAndFree :: Storable a => CInt -> Ptr a -> IO [a]
peekArrayAndFree n p = peekArray (fromIntegral n) p <* free p

foreign import ccall "pango_tab_array_get_positions_in_pixels"
	c_pango_tab_array_get_positions_in_pixels ::
	Ptr PangoTabArray -> IO #type gboolean

pangoTabArrayIntFreeze :: PrimMonad m =>
	PangoTabArrayInt (PrimState m) -> m PangoTabArray
pangoTabArrayIntFreeze (PangoTabArrayInt fta) =
	unsafeIOToPrim $ withForeignPtr fta \pta ->
		makePangoTabArray =<< c_pango_tab_array_freeze pta

pangoTabArrayFixedFreeze :: PrimMonad m =>
	PangoTabArrayFixed (PrimState m) -> m PangoTabArray
pangoTabArrayFixedFreeze (PangoTabArrayFixed fta) =
	unsafeIOToPrim $ withForeignPtr fta \pta ->
		makePangoTabArray =<< c_pango_tab_array_freeze pta

pangoTabArrayThaw :: PrimMonad m => PangoTabArray -> m (Either
	(PangoTabArrayFixed (PrimState m))
	(PangoTabArrayInt (PrimState m)))
pangoTabArrayThaw (PangoTabArray fta) =
	unsafeIOToPrim $ withForeignPtr fta \pta -> do
		px <- c_pango_tab_array_get_positions_in_pixels pta
		c_pango_tab_array_thaw pta >>= case px of
			#{const FALSE} -> (Left <$>) . mkPangoTabArrayFixed
			#{const TRUE} -> (Right <$>) . mkPangoTabArrayInt
			_ -> error "never occur"

foreign import ccall "pango_tab_array_copy" c_pango_tab_array_freeze ::
	Ptr PangoTabArray -> IO (Ptr PangoTabArray)

foreign import ccall "pango_tab_array_copy" c_pango_tab_array_thaw ::
	Ptr PangoTabArray -> IO (Ptr PangoTabArray)
