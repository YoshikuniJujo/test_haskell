{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module System.Glib.SimpleXmlSubsetParser where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.String
import Control.Monad.Primitive
import Data.Word
import Data.Int
import System.Glib.ErrorReporting
import System.Glib.Quarks.Internal

import System.IO.Unsafe

import qualified Data.Text as T
import qualified Data.Text.Foreign as T

#include <glib.h>

newtype GMarkupParseContext s =
	GMarkupParseContext (ForeignPtr (GMarkupParseContext s)) deriving Show

mkGMarkupParseContext :: Ptr (GMarkupParseContext s) -> IO (GMarkupParseContext s)
mkGMarkupParseContext p = GMarkupParseContext
	<$> newForeignPtr p (c_g_markup_parse_context_free p)

foreign import ccall "g_markup_parse_context_free"
	c_g_markup_parse_context_free :: Ptr (GMarkupParseContext s) -> IO ()

gMarkupParseContextParse :: PrimMonad m =>
	GMarkupParseContext (PrimState m) -> T.Text -> m (Either GError ())
gMarkupParseContextParse (GMarkupParseContext fpc) t = unsafeIOToPrim
	$ withForeignPtr fpc \ppc -> T.withCStringLen t \(ct, ctl) -> alloca \pge -> do
		r <- c_g_markup_parse_context_parse ppc ct (fromIntegral ctl) pge
		case r of
			#{const FALSE} -> Left <$> (mkGError =<< peek pge)
			#{const TRUE} -> pure $ Right ()
			_ -> error "never occur"

foreign import ccall "g_markup_parse_context_parse"
	c_g_markup_parse_context_parse ::
	Ptr (GMarkupParseContext s) -> CString -> #{type gssize} ->
	Ptr (Ptr GError) -> IO #{type gboolean}

pattern GErrorMarkup :: GMarkupError -> String -> GError
pattern GErrorMarkup c m <- (gErrorMarkup -> Just (c, m)) where
	GErrorMarkup (GMarkupError c) m =
		GError gMarkupErrorGQuark (fromIntegral c) m

gErrorMarkup :: GError -> Maybe (GMarkupError, String)
gErrorMarkup (GError d c m)
	| d == gMarkupErrorGQuark = Just (GMarkupError $ fromIntegral c, m)
	| otherwise = Nothing

gMarkupErrorGQuark :: GQuark
gMarkupErrorGQuark = unsafePerformIO c_g_markup_error_quark

foreign import ccall "g_markup_error_quark" c_g_markup_error_quark :: IO GQuark

newtype GMarkupError = GMarkupError #{type GMarkupError} deriving Show

pattern GMarkupErrorBadUtf8 :: GMarkupError
pattern GMarkupErrorBadUtf8 <- GMarkupError #{const G_MARKUP_ERROR_BAD_UTF8} where
	GMarkupErrorBadUtf8 = GMarkupError #{const G_MARKUP_ERROR_BAD_UTF8}

pattern GMarkupErrorEmpty :: GMarkupError
pattern GMarkupErrorEmpty <- GMarkupError #{const G_MARKUP_ERROR_EMPTY} where
	GMarkupErrorEmpty = GMarkupError #{const G_MARKUP_ERROR_EMPTY}

pattern GMarkupErrorParse :: GMarkupError
pattern GMarkupErrorParse <- GMarkupError #{const G_MARKUP_ERROR_PARSE} where
	GMarkupErrorParse = GMarkupError #{const G_MARKUP_ERROR_PARSE}

pattern GMarkupErrorUnknownElement :: GMarkupError
pattern GMarkupErrorUnknownElement <- GMarkupError #{const G_MARKUP_ERROR_UNKNOWN_ELEMENT} where
	GMarkupErrorUnknownElement = GMarkupError #{const G_MARKUP_ERROR_UNKNOWN_ELEMENT}

pattern GMarkupErrorUnknownAttribute :: GMarkupError
pattern GMarkupErrorUnknownAttribute <- GMarkupError #{const G_MARKUP_ERROR_UNKNOWN_ATTRIBUTE} where
	GMarkupErrorUnknownAttribute = GMarkupError #{const G_MARKUP_ERROR_UNKNOWN_ATTRIBUTE}

pattern GMarkupErrorInvalidContent :: GMarkupError
pattern GMarkupErrorInvalidContent <- GMarkupError #{const G_MARKUP_ERROR_INVALID_CONTENT} where
	GMarkupErrorInvalidContent = GMarkupError #{const G_MARKUP_ERROR_INVALID_CONTENT}

pattern GMarkupErrorMissingAttribute :: GMarkupError
pattern GMarkupErrorMissingAttribute <- GMarkupError #{const G_MARKUP_ERROR_MISSING_ATTRIBUTE} where
	GMarkupErrorMissingAttribute = GMarkupError #{const G_MARKUP_ERROR_MISSING_ATTRIBUTE}
