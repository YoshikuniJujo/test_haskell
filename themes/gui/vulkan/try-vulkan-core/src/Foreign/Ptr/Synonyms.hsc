module Foreign.Ptr.Synonyms where

import Foreign.Ptr
import Foreign.C.String
import Data.Word
import Data.Int

#include <vulkan/vulkan.h>

type PtrVoid = Ptr ()
type PtrUint32T = Ptr #{type uint32_t}
type PtrFloat = Ptr #{type float}
type PtrCString = Ptr CString
type PtrResult = Ptr #{type VkResult}

type ListUint32T = [#{type uint32_t}]
type ListFloat = [#{type float}]
