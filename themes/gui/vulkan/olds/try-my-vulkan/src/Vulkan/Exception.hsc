{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Exception where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Enum
import Control.Exception
import Control.Exception.Hierarchy
import Data.Int

#include <vulkan/vulkan.h>

enum "Result" ''#{type VkResult} [''Show, ''Read, ''Eq, ''Enum, ''Storable] [
	("Success", #{const VK_SUCCESS}),
	("NotReady", #{const VK_NOT_READY}),
	("Timeout", #{const VK_TIMEOUT}),
	("EventSet", #{const VK_EVENT_SET}),
	("EventReset", #{const VK_EVENT_RESET}),
	("Incomplete", #{const VK_INCOMPLETE}),

	("ErrorOutOfHostMemory", #{const VK_ERROR_OUT_OF_HOST_MEMORY}),
	("ErrorOutOfDeviceMemory", #{const VK_ERROR_OUT_OF_DEVICE_MEMORY}),

	("ErrorExtensionNotPresent", #{const VK_ERROR_EXTENSION_NOT_PRESENT}) ]

type PtrResult = Ptr Result

exceptionHierarchy Nothing (ExType ''Result)

throwUnlessSuccess :: Result -> IO ()
throwUnlessSuccess = \case Success -> pure (); e -> throw e
