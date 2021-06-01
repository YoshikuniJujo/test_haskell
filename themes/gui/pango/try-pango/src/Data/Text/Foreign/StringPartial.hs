{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Text.Foreign.StringPartial (
	CStringPart, emptyOrCStringPart, peekCStringPart, withCStringPart
	) where

import Foreign.Ptr
import Foreign.C.Types

import qualified Data.Text as T
import qualified Data.Text.Foreign as T

type CStringPart = (Ptr CChar, Ptr CChar)

emptyOrCStringPart :: a -> a -> CStringPart -> a
emptyOrCStringPart d f (s, e) | s == e = d | otherwise = f

peekCStringPart :: CStringPart -> IO T.Text
peekCStringPart (s, e) = T.peekCStringLen (s, e `minusPtr` s)

withCStringPart :: T.Text -> (CStringPart -> IO a) -> IO a
withCStringPart t f = T.withCStringLen t \(p, l) -> f (p, p `plusPtr` l)
