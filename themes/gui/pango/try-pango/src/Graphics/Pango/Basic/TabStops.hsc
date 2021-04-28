{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.TabStops where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Control.Arrow
import Control.Monad.Primitive
import Data.Foldable
import Data.Word
import Data.Int
import System.IO.Unsafe

import Graphics.Pango.Types
import Graphics.Pango.Values

#include <pango/pango.h>

foreign import ccall "pango_tab_array_new" c_pango_tab_array_new ::
	CInt -> #{type gboolean} -> IO (Ptr (PangoTabArrayPrim s))

boolToGboolean :: Bool -> #{type gboolean}
boolToGboolean False = #const FALSE
boolToGboolean True = #const TRUE

gbooleanToBool :: #{type gboolean} -> Bool
gbooleanToBool #{const FALSE} = False
gbooleanToBool _ = True

pangoTabArrayDoubleNew ::
	PrimMonad m => m (PangoTabArrayDouble (PrimState m))
pangoTabArrayDoubleNew = unsafeIOToPrim
	$ mkPangoTabArrayDouble =<< c_pango_tab_array_new 1 #{const FALSE}

pangoTabArrayIntNew ::
	PrimMonad m => m (PangoTabArrayInt (PrimState m))
pangoTabArrayIntNew = unsafeIOToPrim
	$ mkPangoTabArrayInt =<< c_pango_tab_array_new 1 #{const TRUE}

pangoTabArrayNew :: PrimMonad m =>
	CInt -> Bool -> m (PangoTabArrayPrim (PrimState m))
pangoTabArrayNew sz px = unsafeIOToPrim
	$ makePangoTabArrayPrim =<< c_pango_tab_array_new sz (boolToGboolean px)

pangoTabArrayDoubleSetTab :: PrimMonad m =>
	PangoTabArrayDouble (PrimState m) -> CInt -> Double -> m ()
pangoTabArrayDoubleSetTab (PangoTabArrayDouble fta) idx x = unsafeIOToPrim
	$ withForeignPtr fta \pta -> do
		sz <- c_pango_tab_array_get_size_prim pta
		if idx < sz
		then c_pango_tab_array_set_tab pta idx #{const PANGO_TAB_LEFT}
			. round $ x * #{const PANGO_SCALE}
		else do	lst <- tempPangoTabArrayGetTab pta (sz - 1)
			let	Just (sz', tss) = calculateDouble sz idx (fromIntegral lst / #{const PANGO_SCALE}) x
			c_pango_tab_array_resize pta sz'
			for_ (zip [sz .. sz' - 1] tss) \(i, xx) ->
				c_pango_tab_array_set_tab pta i #{const PANGO_TAB_LEFT}
					. round $ xx * #{const PANGO_SCALE}

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
	Ptr (PangoTabArrayPrim s) -> IO CInt

foreign import ccall "pango_tab_array_get_size" c_pango_tab_array_get_size ::
	Ptr PangoTabArray -> IO CInt

foreign import ccall "pango_tab_array_resize" c_pango_tab_array_resize ::
	Ptr (PangoTabArrayPrim s) -> CInt -> IO ()

pangoTabArrayResize :: PrimMonad m =>
	PangoTabArrayPrim (PrimState m) -> CInt -> m ()
pangoTabArrayResize (PangoTabArrayPrim fpta) sz = unsafeIOToPrim
	$ withForeignPtr fpta \pta -> c_pango_tab_array_resize pta sz

foreign import ccall "pango_tab_array_set_tab" c_pango_tab_array_set_tab ::
	Ptr (PangoTabArrayPrim s) -> CInt -> #{type PangoTabAlign} -> CInt -> IO ()

pangoTabArraySetTab :: PrimMonad m =>
	PangoTabArrayPrim (PrimState m) -> CInt -> PangoTabAlign -> CInt -> m ()
pangoTabArraySetTab (PangoTabArrayPrim fpta) idx (PangoTabAlign algn) loc = unsafeIOToPrim
	$ withForeignPtr fpta \pta -> c_pango_tab_array_set_tab pta idx algn loc

tempPangoTabArrayGetTab :: Ptr (PangoTabArrayPrim s) -> CInt -> IO CInt
tempPangoTabArrayGetTab pta idx = alloca \px -> do
	c_pango_tab_array_get_tab_prim pta idx nullPtr px
	peek px

pangoTabArrayGetTab :: PangoTabArray -> CInt -> Maybe (Either Double CInt)
pangoTabArrayGetTab PangoTabArrayNull _ = Nothing
pangoTabArrayGetTab (PangoTabArray fta) idx = unsafePerformIO
	$ Just <$> withForeignPtr fta \pta -> alloca \loc -> do
		px <- c_pango_tab_array_get_positions_in_pixels pta
		c_pango_tab_array_get_tab pta idx nullPtr loc
		(<$> peek loc) case px of
			#{const FALSE} ->
				Left . (/ #{const PANGO_SCALE}) . fromIntegral
			#{const TRUE} -> Right
			_ -> error "never occur"

foreign import ccall "pango_tab_array_get_tab" c_pango_tab_array_get_tab_prim ::
	Ptr (PangoTabArrayPrim s) -> CInt -> Ptr #{type PangoTabAlign} -> Ptr CInt -> IO ()

foreign import ccall "pango_tab_array_get_tab" c_pango_tab_array_get_tab ::
	Ptr PangoTabArray -> CInt -> Ptr #{type PangoTabAlign} -> Ptr CInt -> IO ()

pangoTabArrayGetTabs :: PangoTabArray -> Maybe (Either [Double] [CInt])
pangoTabArrayGetTabs PangoTabArrayNull = Nothing
pangoTabArrayGetTabs (PangoTabArray fta) = unsafePerformIO
	$ Just <$> withForeignPtr fta \pta -> alloca \locs -> do
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

pangoTabArrayDoubleFreeze :: PrimMonad m =>
	PangoTabArrayDouble (PrimState m) -> m PangoTabArray
pangoTabArrayDoubleFreeze (PangoTabArrayDouble fta) =
	unsafeIOToPrim $ withForeignPtr fta \pta ->
		makePangoTabArray =<< c_pango_tab_array_freeze pta

pangoTabArrayIntFreeze :: PrimMonad m =>
	PangoTabArrayInt (PrimState m) -> m PangoTabArray
pangoTabArrayIntFreeze (PangoTabArrayInt fta) =
	unsafeIOToPrim $ withForeignPtr fta \pta ->
		makePangoTabArray =<< c_pango_tab_array_freeze pta

pangoTabArrayFreeze :: PrimMonad m =>
	PangoTabArrayPrim (PrimState m) -> m PangoTabArray
pangoTabArrayFreeze (PangoTabArrayPrim fpta) = unsafeIOToPrim
	$ withForeignPtr fpta \pta -> makePangoTabArray =<< c_pango_tab_array_freeze pta

pangoTabArrayThaw :: PrimMonad m => PangoTabArray -> m (Maybe (Either
	(PangoTabArrayDouble (PrimState m))
	(PangoTabArrayInt (PrimState m))))
pangoTabArrayThaw PangoTabArrayNull = pure Nothing
pangoTabArrayThaw (PangoTabArray fta) =
	unsafeIOToPrim $ Just <$> withForeignPtr fta \pta -> do
		px <- c_pango_tab_array_get_positions_in_pixels pta
		c_pango_tab_array_thaw pta >>= case px of
			#{const FALSE} -> (Left <$>) . mkPangoTabArrayDouble
			#{const TRUE} -> (Right <$>) . mkPangoTabArrayInt
			_ -> error "never occur"

foreign import ccall "pango_tab_array_copy" c_pango_tab_array_freeze ::
	Ptr (PangoTabArrayPrim s) -> IO (Ptr PangoTabArray)

foreign import ccall "pango_tab_array_copy" c_pango_tab_array_thaw ::
	Ptr PangoTabArray -> IO (Ptr (PangoTabArrayPrim s))
