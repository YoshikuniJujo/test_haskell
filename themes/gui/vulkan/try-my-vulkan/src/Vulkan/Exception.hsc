{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Exception where

import Foreign.C.Enum
import Control.Exception.Hierarchy
import Data.Int

#include <vulkan/vulkan.h>

enum "Result" ''#{type VkResult} [''Show, ''Read, ''Eq] [
	("Success", #{const VK_SUCCESS}),
	("NotReady", #{const VK_NOT_READY}),
	("Timeout", #{const VK_TIMEOUT}),
	("EventSet", #{const VK_EVENT_SET}),
	("EventReset", #{const VK_EVENT_RESET}),
	("Incomplete", #{const VK_INCOMPLETE})
	]

exceptionHierarchy Nothing (ExType ''Result)
