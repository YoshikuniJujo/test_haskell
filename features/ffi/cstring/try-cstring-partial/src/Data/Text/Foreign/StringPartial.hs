{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Text.Foreign.StringPartial (
	CStringPart, peekCStringPart, withCStringPart ) where

import Foreign.Ptr
import Foreign.C.StringPartial (CStringPart)

import qualified Data.Text as T
import qualified Data.Text.Foreign as T

peekCStringPart :: CStringPart -> IO T.Text
peekCStringPart (s, e) = T.peekCStringLen (s, e `minusPtr` s)

withCStringPart :: T.Text -> (CStringPart -> IO a) -> IO a
withCStringPart t f = T.withCStringLen t \(p, l) -> f (p, p `plusPtr` l)
