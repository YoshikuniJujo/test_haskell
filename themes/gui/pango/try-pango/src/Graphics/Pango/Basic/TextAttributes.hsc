{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.TextAttributes where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String
import Control.Monad.Primitive
import Data.Word
import Data.Int
import Data.Char
import System.IO.Unsafe

import Data.Text.CString

import System.Glib.ErrorReporting
import System.Glib.SimpleXmlSubsetParser

import qualified Data.Text as T
import qualified Data.Text.Foreign as T

import Graphics.Pango.Basic.TextAttributes.Template

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

pangoMarkupParserNew :: PrimMonad m => Maybe Char -> m (GMarkupParseContext (PrimState m))
pangoMarkupParserNew am = unsafeIOToPrim
	$ mkGMarkupParseContext =<< c_pango_markup_parser_new (toGunichar am)

foreign import ccall "pango_markup_parser_new" c_pango_markup_parser_new ::
	#{type gunichar} -> IO (Ptr (GMarkupParseContext s))

pangoMarkupParserFinish :: PrimMonad m => GMarkupParseContext (PrimState m) ->
	m (Either GError (PangoAttrList, T.Text, Maybe Char))
pangoMarkupParserFinish (GMarkupParseContext fpc) = unsafeIOToPrim
	$ withForeignPtr fpc \ppc -> alloca \ppal -> alloca \pt -> alloca \pac -> alloca \pge -> do
		r <- c_pango_markup_parser_finish ppc ppal pt pac pge
		case r of
			#{const FALSE} -> Left <$> (mkGError =<< peek pge)
			#{const TRUE} -> (Right <$>) $ (,,)
				<$> (mkPangoAttrList =<< peek ppal)
				<*> (peekCStringText =<< peek pt)
				<*> (fromGunichar <$> peek pac)
			_ -> error "never occur"

foreign import ccall "pango_markup_parser_finish"
	c_pango_markup_parser_finish ::
	Ptr (GMarkupParseContext s) -> Ptr (Ptr PangoAttrList) -> Ptr CString ->
	Ptr #{type gunichar} -> Ptr (Ptr GError) -> IO #{type gboolean}

mkMemberAttrType "PangoAttrInvalid" #{const PANGO_ATTR_INVALID}
mkMemberAttrType "PangoAttrLanguage" #{const PANGO_ATTR_LANGUAGE}
mkMemberAttrType "PangoAttrFamily" #{const PANGO_ATTR_FAMILY}
mkMemberAttrType "PangoAttrStyle" #{const PANGO_ATTR_STYLE}

newtype PangoAttribute s = PangoAttribute (ForeignPtr (PangoAttribute s))
	deriving Show

mkPangoAttribute :: Ptr (PangoAttribute s) -> IO (PangoAttribute s)
mkPangoAttribute p =
	PangoAttribute <$> newForeignPtr p (c_pango_attribute_destroy p)

foreign import ccall "pango_attribute_destroy" c_pango_attribute_destroy ::
	Ptr (PangoAttribute s) -> IO ()

pangoAttributeSetStartIndex, pangoAttributeSetEndIndex :: PrimMonad m =>
	PangoAttribute (PrimState m) -> CUInt -> m ()
pangoAttributeSetStartIndex (PangoAttribute fa) si = unsafeIOToPrim
	$ withForeignPtr fa \pa -> #{poke PangoAttribute, start_index} pa si

pangoAttributeSetEndIndex (PangoAttribute fa) ei = unsafeIOToPrim
	$ withForeignPtr fa \pa -> #{poke PangoAttribute, end_index} pa ei

class PangoAttributeValue v where
	pangoAttrNew :: PrimMonad m => v -> m (PangoAttribute (PrimState m))

data Family = Family String deriving Show

instance PangoAttributeValue Family where
	pangoAttrNew (Family f) = pangoAttrFamilyNew f

pangoAttrFamilyNew :: PrimMonad m => String -> m (PangoAttribute (PrimState m))
pangoAttrFamilyNew f =
	unsafeIOToPrim $ mkPangoAttribute =<< withCString f c_pango_attr_family_new

foreign import ccall "pango_attr_family_new" c_pango_attr_family_new ::
	CString -> IO (Ptr (PangoAttribute s))

data Size = Size Double | AbsoluteSize Double deriving Show

instance PangoAttributeValue Size where
	pangoAttrNew = pangoAttrSizeNew

pangoAttrSizeNew :: PrimMonad m => Size -> m (PangoAttribute (PrimState m))
pangoAttrSizeNew = unsafeIOToPrim . (mkPangoAttribute =<<) . \case
	Size s -> c_pango_attr_size_new . round $ s * #{const PANGO_SCALE}
	AbsoluteSize a -> c_pango_attr_size_new_absolute . round $ a * #{const PANGO_SCALE}

foreign import ccall "pango_attr_size_new" c_pango_attr_size_new ::
	CInt -> IO (Ptr (PangoAttribute s))

foreign import ccall "pango_attr_size_new_absolute" c_pango_attr_size_new_absolute ::
	CInt -> IO (Ptr (PangoAttribute s))

newtype PangoAttrListPrim s = PangoAttrListPrim (ForeignPtr (PangoAttrListPrim s)) deriving Show

mkPangoAttrListPrim :: Ptr (PangoAttrListPrim s) -> IO (PangoAttrListPrim s)
mkPangoAttrListPrim p = PangoAttrListPrim <$> newForeignPtr p (c_pango_attr_list_prim_unref p)

foreign import ccall "pango_attr_list_unref" c_pango_attr_list_prim_unref ::
	Ptr (PangoAttrListPrim s) -> IO ()

pangoAttrListNew :: PrimMonad m => m (PangoAttrListPrim (PrimState m))
pangoAttrListNew = unsafeIOToPrim $ mkPangoAttrListPrim =<< c_pango_attr_list_new

foreign import ccall "pango_attr_list_new" c_pango_attr_list_new ::
	IO (Ptr (PangoAttrListPrim s))

pangoAttrListFreeze ::
	PrimMonad m => PangoAttrListPrim (PrimState m) -> m PangoAttrList
pangoAttrListFreeze (PangoAttrListPrim fal) = unsafeIOToPrim
	$ mkPangoAttrList =<< withForeignPtr fal c_pango_attr_list_freeze

foreign import ccall "pango_attr_list_copy" c_pango_attr_list_freeze ::
	Ptr (PangoAttrListPrim s) -> IO (Ptr PangoAttrList)

pangoAttrListThaw ::
	PrimMonad m => PangoAttrList -> m (PangoAttrListPrim (PrimState m))
pangoAttrListThaw (PangoAttrList fal) = unsafeIOToPrim
	$ mkPangoAttrListPrim =<< withForeignPtr fal c_pango_attr_list_thaw

foreign import ccall "pango_attr_list_copy" c_pango_attr_list_thaw ::
	Ptr PangoAttrList -> IO (Ptr (PangoAttrListPrim s))

pangoAttrListInsert :: PrimMonad m =>
	PangoAttrListPrim (PrimState m) -> PangoAttribute (PrimState m) -> m ()
pangoAttrListInsert (PangoAttrListPrim fal) (PangoAttribute fa) = unsafeIOToPrim
	$ withForeignPtr fal \pal -> withForeignPtr fa \pa -> do
		pa' <- c_pango_attribute_copy pa
		c_pango_attr_list_insert pal pa'

foreign import ccall "pango_attr_list_insert" c_pango_attr_list_insert ::
	Ptr (PangoAttrListPrim s) -> Ptr (PangoAttribute s) -> IO ()

foreign import ccall "pango_attribute_copy" c_pango_attribute_copy ::
	Ptr (PangoAttribute s) -> IO (Ptr (PangoAttribute s))
