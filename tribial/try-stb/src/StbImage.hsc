{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module StbImage where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.C.String
import Foreign.C.Enum
import Control.Monad.Cont
import Data.Word
import Data.Int

import qualified Data.ByteString as BS

#define STB_IMAGE_IMPLEMENTATION
#include <stb/stb_image.h>

enum "Stbi" ''#{type int} [''Show] [
	("StbiDefault", #{const STBI_default}),
	("StbiGrey", #{const STBI_grey}),
	("StbiGreyAlpha", #{const STBI_grey_alpha}),
	("StbiRgb", #{const STBI_rgb}),
	("StbiRgbAlpha", #{const STBI_rgb_alpha}) ]

stbiLoad :: String -> Stbi -> IO Image
stbiLoad fp (Stbi comp) = ($ pure) $ runContT do
	cfp <- ContT $ withCString fp
	w <- ContT alloca
	h <- ContT alloca
	c <- ContT alloca
	lift do	r <- c_stbi_load cfp w h c comp
		Image <$> peek w <*> peek h <*> peek c <*> pure comp <*> pure r

data Image = Image {
	imageWidth :: #{type int},
	imageHeight :: #{type int},
	imageChannelsImage :: #{type int},
	imageChannelsMemory :: #{type int},
	imagePixels :: Ptr #{type stbi_uc} }
	deriving Show

getImageBytes :: Image -> IO BS.ByteString
getImageBytes Image {
	imageWidth = w, imageHeight = h, imageChannelsMemory = c,
	imagePixels = pps } =
	BS.pack <$> peekArray (fromIntegral $ w * h * c) pps

foreign import capi "stb/stb_image.h stbi_load" c_stbi_load ::
	CString -> Ptr #{type int} -> Ptr #{type int} -> Ptr #{type int} ->
	#{type int} -> IO (Ptr #{type stbi_uc})

sampleSbti :: IO BS.ByteString
sampleSbti = getImageBytes
	=<< stbiLoad "../../files/images/texture.jpg" StbiRgbAlpha
