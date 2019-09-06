module CheckCode where

import Foreign.Marshal
import Foreign.C.String

withMultiCStrings :: [String] -> ([CString] -> IO a) -> IO a
-- withMultiCStrings [] act = act []
withMultiCStrings [] = ($ [])
-- withMultiCStrings (s : ss) act = withCString s $ \cs -> withMultiCStrings ss $ \css -> act (cs : css)
-- withMultiCStrings (s : ss) act = withCString s $ (withMultiCStrings ss $) . (act .) . (:)
withMultiCStrings (s : ss) = withCString s . ((withMultiCStrings ss $) .) . (. (:)) . (.)

some a b = withCString a . ((b $) .) . (. (:)) . (.)

withMultiCStrings' :: [String] -> ([CString] -> IO a) -> IO a
withMultiCStrings' = Prelude.foldr some ($ [])

with2CStrings :: (String, String) -> ((CString, CString) -> IO a) -> IO a
with2CStrings (s, t) act = withCString s $ \cs -> withCString t $ \ct -> act (cs, ct)

with3CStrings :: (String, (String, String)) -> ((CString, (CString, CString)) -> IO a) -> IO a
with3CStrings (s, ss) act = withCString s $ \cs -> with2CStrings ss $ \css -> act (cs, css)
