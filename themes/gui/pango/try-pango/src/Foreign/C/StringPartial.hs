{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Foreign.C.StringPartial (
	CStringPart, withCStringPart, peekCStringPart,
	emptyOrCStringPart ) where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

type CStringPart = (Ptr CChar, Ptr CChar)

withCStringPart :: String -> (CStringPart -> IO a) -> IO a
withCStringPart str f = withCStringLen str \(p, l) -> f (p, p `plusPtr` l)

peekCStringPart :: CStringPart -> IO String
peekCStringPart (s, e) = peekCStringLen (s, e `minusPtr` s)

emptyOrCStringPart :: a -> a -> CStringPart -> a
emptyOrCStringPart d f (s, e) | s == e = d | otherwise = f
