{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.PhysicalDevice where

import Foreign.Marshal
import Foreign.Storable
import Control.Monad.Cont
import Data.Word

import Vulkan
import Vulkan.Base
import Vulkan.Exception
import Vulkan.Exception.Enum

import qualified Vulkan.PhysicalDevice.Core as C

enumerate :: Instance -> IO [PhysicalDevice]
enumerate (Instance ist) = ($ pure) . runContT $ map PhysicalDevice <$> do
	pdvcc <- ContT alloca
	(fromIntegral -> dvcc) <- lift do
		r <- C.enumerate ist pdvcc NullPtr
		throwUnlessSuccess $ Result r
		peek pdvcc
	pdvcs <- ContT $ allocaArray dvcc
	lift do	r <- C.enumerate ist pdvcc pdvcs
		throwUnlessSuccess $ Result r
		peekArray dvcc pdvcs

data Properties = Properties {
	propertiesApiVersion :: ApiVersion,
	propertiesDriverVersion :: Word32,
	propertiesVendorId :: Word32,
	propertiesDeviceId :: Word32
	}
	deriving Show
