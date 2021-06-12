{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.ScriptsAndLanguages.PangoScript.Internal (
	-- * FUNCTION
	pangoScriptForUnichar, pangoScriptGetSampleLanguage, pangoScriptForText,
	-- * ENUM
	module Graphics.Pango.Basic.ScriptsAndLanguages.PangoScript.Enum,
	) where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String
import Control.Exception
import Data.Traversable
import Data.Bool
import Data.Word
import Data.Int
import Data.Char
import System.IO.Unsafe

import Graphics.Pango.Basic.ScriptsAndLanguages.PangoLanguage.Internal
import Graphics.Pango.Basic.ScriptsAndLanguages.PangoScript.Enum

import qualified Data.Text as T
import qualified Data.Text.Foreign as T
import qualified Data.Text.Foreign.StringPartial as T

#include <pango/pango.h>

pangoScriptForUnichar :: Char -> PangoScript
pangoScriptForUnichar c = unsafePerformIO
	$ PangoScript <$> c_pango_script_for_unichar (fromIntegral $ ord c)

foreign import ccall "pango_script_for_unichar" c_pango_script_for_unichar ::
	#{type gunichar} -> IO #{type PangoScript}

pangoScriptGetSampleLanguage :: PangoScript -> IO (Maybe PangoLanguage)
pangoScriptGetSampleLanguage (PangoScript s) =
	nullable Nothing (Just . PangoLanguage_)
		<$> c_pango_script_get_sample_language s

foreign import ccall "pango_script_get_sample_language"
	c_pango_script_get_sample_language ::
	#{type PangoScript} -> IO (Ptr PangoLanguage)

pattern NullPtr :: Ptr a
pattern NullPtr <- ((== nullPtr) -> True) where NullPtr = nullPtr

nullable :: b -> (Ptr a -> b) -> Ptr a -> b
nullable d f = \case NullPtr -> d; p -> f p

pangoScriptForText :: T.Text -> [(T.Text, PangoScript)]
pangoScriptForText t = unsafePerformIO $ withPangoScriptIter t \i -> do
	rss <- pangoScriptIterGetRanges i
	for rss \(r, s) -> (, s) <$> T.peekCStringPart r

data PangoScriptIter

withPangoScriptIter :: T.Text -> (Ptr PangoScriptIter -> IO a) -> IO a
withPangoScriptIter t f = T.withCStringLen t \(cs, l) -> bracket
	(c_pango_script_iter_new cs $ fromIntegral l) c_pango_script_iter_free f

foreign import ccall "pango_script_iter_new" c_pango_script_iter_new ::
	CString -> CInt -> IO (Ptr PangoScriptIter)

foreign import ccall "pango_script_iter_free" c_pango_script_iter_free ::
	Ptr PangoScriptIter -> IO ()

pangoScriptIterGetRanges :: Ptr PangoScriptIter -> IO [(T.CStringPart, PangoScript)]
pangoScriptIterGetRanges i = unsafeInterleaveIO do
	rs@(r, _) <- pangoScriptIterGetRange i
	(\r' e f -> T.emptyOrCStringPart e f r') r (pure [])
		$ pangoScriptIterNext i
			>>= bool (pure [rs]) ((rs :) <$> pangoScriptIterGetRanges i)

pangoScriptIterGetRange :: Ptr PangoScriptIter -> IO (T.CStringPart, PangoScript)
pangoScriptIterGetRange i = alloca \st -> alloca \ed -> alloca \s -> do
	c_pango_script_iter_get_range i st ed s
	(,) <$> ((,) <$> peek st <*> peek ed) <*> (PangoScript <$> peek s)

foreign import ccall "pango_script_iter_get_range" c_pango_script_iter_get_range ::
	Ptr PangoScriptIter -> Ptr CString -> Ptr CString -> Ptr #{type PangoScript} -> IO ()

pangoScriptIterNext :: Ptr PangoScriptIter -> IO Bool
pangoScriptIterNext i = (<$> c_pango_script_iter_next i) \case
	#{const FALSE} -> False; #{const TRUE} -> True
	_ -> error "never occur"

foreign import ccall "pango_script_iter_next" c_pango_script_iter_next ::
	Ptr PangoScriptIter -> IO #{type gboolean}
