{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.C.Types
import Foreign.C.String
import Control.Exception
import Data.Bool
import Data.Int
import System.IO.Unsafe
import System.Environment

import Ptr
import Exception

#include <GL/glx.h>

newtype Display = Display (Ptr Display) deriving Show

xOpenDisplay :: Maybe String -> IO Display
xOpenDisplay mdn = do
	dn <- case mdn of Nothing -> getEnv "DISPLAY"; Just n -> pure n
	display dn =<< case mdn of
		Nothing -> c_XOpenDisplay nullPtr
		Just n -> withCString n c_XOpenDisplay

display :: String -> Ptr Display -> IO Display
display dn = \case
	NullPtr -> throw . XOpenDisplayError $ xOpenDisplayErrorMessage dn
	p -> pure $ Display p

xOpenDisplayErrorMessage :: String -> String
xOpenDisplayErrorMessage = ("Error: couldn't open display " ++)

foreign import ccall "XOpenDisplay" c_XOpenDisplay :: CString -> IO (Ptr Display)

xCloseDisplay :: Display -> IO ()
xCloseDisplay (Display p) = c_XCloseDisplay p

foreign import ccall "XCloseDisplay" c_XCloseDisplay :: Ptr Display -> IO ()

newtype GlXContext = GlXContext (Ptr GlXContext) deriving Show

newtype ScreenNumber = ScreenNumber CInt deriving Show

xDefaultScreen :: Display -> ScreenNumber
xDefaultScreen (Display p) =
	unsafePerformIO $ ScreenNumber <$> c_XDefaultScreen p

foreign import ccall "XDefaultScreen" c_XDefaultScreen :: Ptr Display -> IO CInt

newtype XVisualInfo = XVisualInfo (Ptr XVisualInfo) deriving Show

data GlxAttributes = GlxAttributes {
	glxBufferSize :: CInt, glxLevel :: CInt, glxRgba :: Bool,
	glxDoublebuffer :: Bool, glxStereo :: Bool, glxAuxBuffers :: CInt,
	glxRedSize :: CInt, glxGreenSize :: CInt, glxBlueSize :: CInt,
	glxAlphaSize :: CInt,
	glxDepthSize :: CInt, glxStencilSize :: CInt,
	glxAccumRedSize :: CInt, glxAccumGreenSize :: CInt,
	glxAccumBlueSize :: CInt, glxAccumAlphaSize :: CInt }
	deriving Show

defaultGlxAttributes :: GlxAttributes
defaultGlxAttributes = GlxAttributes {
	glxBufferSize = 0, glxLevel = 0, glxRgba = False,
	glxDoublebuffer = False, glxStereo = False, glxAuxBuffers = 0,
	glxRedSize = 0, glxGreenSize = 0, glxBlueSize = 0, glxAlphaSize = 0,
	glxDepthSize = 0, glxStencilSize = 0,
	glxAccumRedSize = 0, glxAccumGreenSize = 0,
	glxAccumBlueSize = 0, glxAccumAlphaSize = 0 }

glxAttributeList :: GlxAttributes -> [CInt]
glxAttributeList GlxAttributes {
	glxBufferSize = bs, glxLevel = lvl, glxRgba = rgba,
	glxDoublebuffer = db, glxStereo = str, glxAuxBuffers = ab,
	glxRedSize = rs, glxGreenSize = gs, glxBlueSize = bls, glxAlphaSize = as,
	glxDepthSize = ds, glxStencilSize = ss,
	glxAccumRedSize = ars, glxAccumGreenSize = ags,
	glxAccumBlueSize = abls, glxAccumAlphaSize = aas } =
	[#{const GLX_BUFFER_SIZE}, bs, #{const GLX_LEVEL}, lvl] ++
	bool [] [#{const GLX_RGBA}] rgba ++
	bool [] [#{const GLX_DOUBLEBUFFER}] db ++
	bool [] [#{const GLX_STEREO}] str ++ [
		#{const GLX_AUX_BUFFERS}, ab,
		#{const GLX_RED_SIZE}, rs, #{const GLX_GREEN_SIZE}, gs,
		#{const GLX_BLUE_SIZE}, bls, #{const GLX_ALPHA_SIZE}, as,
		#{const GLX_DEPTH_SIZE}, ds, #{const GLX_STENCIL_SIZE}, ss,
		#{const GLX_ACCUM_RED_SIZE}, ars,
		#{const GLX_ACCUM_GREEN_SIZE}, ags,
		#{const GLX_ACCUM_BLUE_SIZE}, abls,
		#{const GLX_ACCUM_ALPHA_SIZE}, aas, #{const None} ]

glXChooseVisualWith :: Display ->
	ScreenNumber -> GlxAttributes -> (XVisualInfo -> IO a) -> IO a
glXChooseVisualWith dpy scr attrs act =
	(<*) <$> act <*> xFreeXVisualInfo =<< glXChooseVisual dpy scr attrs

glXChooseVisual :: Display -> ScreenNumber -> GlxAttributes -> IO XVisualInfo
glXChooseVisual (Display pd) (ScreenNumber sn) attrs = XVisualInfo
	<$> withArray (glxAttributeList attrs) \pa -> c_glXChooseVisual pd sn pa

foreign import ccall "glXChooseVisual" c_glXChooseVisual ::
	Ptr Display -> CInt -> Ptr CInt -> IO (Ptr XVisualInfo)

xFreeXVisualInfo :: XVisualInfo -> IO ()
xFreeXVisualInfo (XVisualInfo p) = c_XFreeXVisualInfo p

foreign import ccall "XFree" c_XFreeXVisualInfo :: Ptr XVisualInfo -> IO ()

boolToCbool :: Bool -> #{type Bool}
boolToCbool = bool #{const False} #{const True}

cboolToBool :: #{type Bool} -> Bool
cboolToBool = \case
	#{const False} -> False; #{const True} -> True
	_ -> error "cboolToBool: not False nor True"

getGlXContext :: Maybe GlXContext -> Ptr GlXContext
getGlXContext = \case Nothing -> NullPtr; Just (GlXContext pc) -> pc


glXCreateContext ::
	Display -> XVisualInfo -> Maybe GlXContext -> Bool -> IO GlXContext
glXCreateContext (Display pd) (XVisualInfo pv) ctx dr = GlXContext
	<$> c_glXCreateContext pd pv (getGlXContext ctx) (boolToCbool dr)

foreign import ccall "glXCreateContext" c_glXCreateContext ::
	Ptr Display -> Ptr XVisualInfo -> Ptr GlXContext -> #{type Bool} ->
	IO (Ptr GlXContext)

glXDestroyContext :: Display -> GlXContext -> IO ()
glXDestroyContext (Display pd) (GlXContext pc) = c_glXDestroyContext pd pc

foreign import ccall "glXDestroyContext"
	c_glXDestroyContext :: Ptr Display -> Ptr GlXContext -> IO ()

newtype GlXDrawable = GlXDrawable (Ptr GlXDrawable) deriving Show

getGlXDrawable :: Maybe GlXDrawable -> Ptr GlXDrawable
getGlXDrawable = \case Nothing -> NullPtr; Just (GlXDrawable p) -> p

glXMakeCurrent :: Display -> Maybe GlXDrawable -> Maybe GlXContext -> IO Bool
glXMakeCurrent (Display pd) md mc = cboolToBool
	<$> c_glXMakeCurrent pd (getGlXDrawable md) (getGlXContext mc)

foreign import ccall "glXMakeCurrent" c_glXMakeCurrent ::
	Ptr Display -> Ptr GlXDrawable -> Ptr GlXContext -> IO #{type Bool}
