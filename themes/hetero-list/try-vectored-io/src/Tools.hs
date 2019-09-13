{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tools (for2M_, sizeOfPtr, errorWithExpression) where

import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable, sizeOf)
import Control.Monad (forM_)

sizeOfPtr :: forall a . Storable a => Ptr a -> Int
sizeOfPtr _ = sizeOf @a undefined

errorWithExpression :: HasCallStack => String -> String -> a
errorWithExpression fun msg = errorWithoutStackTrace $
	"\n\nexpression: " ++ fun ++ "\nmessage   : " ++ msg ++ "\n\n" ++
	prettyCallStack callStack

for2M_ :: Monad m => [a] -> [b] -> (a -> b -> m c) -> m ()
for2M_ xs ys act = forM_ (xs `zip` ys) $ uncurry act
