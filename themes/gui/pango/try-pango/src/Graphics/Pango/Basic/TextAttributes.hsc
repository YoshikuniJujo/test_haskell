{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.TextAttributes where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String
import Data.Word
import Data.Int
import Data.Char
import System.IO.Unsafe

import Data.Text.CString

import System.Glib.ErrorReporting

import qualified Data.Text as T
import qualified Data.Text.Foreign as T

#include <pango/pango.h>

newtype PangoAttrList = PangoAttrList (ForeignPtr PangoAttrList) deriving Show

mkPangoAttrList :: Ptr PangoAttrList -> IO PangoAttrList
mkPangoAttrList p = PangoAttrList <$> newForeignPtr p (c_pango_attr_list_unref p)

foreign import ccall "pango_attr_list_unref" c_pango_attr_list_unref ::
	Ptr PangoAttrList -> IO ()

pangoParseMarkup :: T.Text -> Maybe Char -> Either GError (PangoAttrList, T.Text, Maybe Char)
pangoParseMarkup mt am = unsafePerformIO
	$ T.withCStringLen mt \(cmt, cmtl) -> alloca \ppal -> alloca \pt -> alloca \pac -> alloca \pge -> do
		r <- c_pango_parse_markup cmt (fromIntegral cmtl) (toGunichar am) ppal pt pac pge
		case r of
			#{const FALSE} -> Left <$> (mkGError =<< peek pge)
			#{const TRUE} -> (Right <$>) $ (,,)
				<$> (mkPangoAttrList =<< peek ppal)
				<*> (peekCStringText =<< peek pt)
				<*> (fromGunichar <$> peek pac)
			_ -> error "never occur"

toGunichar :: Maybe Char -> #{type gunichar}
toGunichar = \case Nothing -> 0; Just c -> fromIntegral $ ord c

fromGunichar :: #{type gunichar} -> Maybe Char
fromGunichar = \case 0 -> Nothing; uc -> Just . chr $ fromIntegral uc

foreign import ccall "pango_parse_markup" c_pango_parse_markup ::
	CString -> CInt -> #{type gunichar} ->
		Ptr (Ptr PangoAttrList) -> Ptr CString -> Ptr #{type gunichar} ->
		Ptr (Ptr GError) -> IO #{type gboolean}
