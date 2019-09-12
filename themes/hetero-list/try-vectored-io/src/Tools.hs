{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tools (sizeOfPtr, errorWithFunName) where

import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable, sizeOf)

sizeOfPtr :: forall a . Storable a => Ptr a -> Int
sizeOfPtr _ = sizeOf @a undefined

errorWithFunName :: HasCallStack => String -> String -> a
errorWithFunName fun msg = errorWithoutStackTrace $
	"\n\nfunction: " ++ fun ++ "\nmessage:  " ++ msg ++ "\n\n" ++
	prettyCallStack callStack
