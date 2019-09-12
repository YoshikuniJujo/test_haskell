{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tools (sizeOfPtr, errorWithExpression) where

import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable, sizeOf)

sizeOfPtr :: forall a . Storable a => Ptr a -> Int
sizeOfPtr _ = sizeOf @a undefined

errorWithExpression :: HasCallStack => String -> String -> a
errorWithExpression fun msg = errorWithoutStackTrace $
	"\n\nexpression: " ++ fun ++ "\nmessage   : " ++ msg ++ "\n\n" ++
	prettyCallStack callStack
