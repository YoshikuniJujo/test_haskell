{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module System.Glib.SimpleXmlSubsetParser (
	-- * TYPE
	GMarkupParseContext(..), mkGMarkupParseContext,

	-- * FUNCTION
	gMarkupParseContextParse,

	-- * G MARKUP ERROR
	pattern GErrorMarkup,
	pattern GMarkupErrorBadUtf8,
	pattern GMarkupErrorEmpty,
	pattern GMarkupErrorParse,
	pattern GMarkupErrorUnknownElement,
	pattern GMarkupErrorUnknownAttribute,
	pattern GMarkupErrorInvalidContent,
	pattern GMarkupErrorMissingAttribute
	) where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.String
import Foreign.C.Enum
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

enum "GMarkupError" ''#{type GMarkupError} [''Show] [
	("GMarkupErrorBadUtf8", #{const G_MARKUP_ERROR_BAD_UTF8}),
	("GMarkupErrorEmpty", #{const G_MARKUP_ERROR_EMPTY}),
	("GMarkupErrorParse", #{const G_MARKUP_ERROR_PARSE}),
	("GMarkupErrorUnknownElement", #{const G_MARKUP_ERROR_UNKNOWN_ELEMENT}),
	("GMarkupErrorUnknownAttribute",
		#{const G_MARKUP_ERROR_UNKNOWN_ATTRIBUTE}),
	("GMarkupErrorInvalidContent", #{const G_MARKUP_ERROR_INVALID_CONTENT}),
	("GMarkupErrorMissingAttribute",
		#{const G_MARKUP_ERROR_MISSING_ATTRIBUTE}) ]

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
