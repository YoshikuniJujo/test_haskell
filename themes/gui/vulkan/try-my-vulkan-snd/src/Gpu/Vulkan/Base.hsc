{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Base where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String
import Control.Arrow
import Control.Monad.Cont
import Data.Word
import Data.Int

import qualified Data.Text as Txt
import qualified Data.Text.Foreign as Txt

#include <vulkan/vulkan.h>

stringToCString :: String -> ContT r IO CString
stringToCString = ContT . withCString

stringListToCStringArray :: [String] -> ContT r IO (Ptr CString)
stringListToCStringArray strs = do
	cstrl <- stringToCString `mapM` strs
	pcstra <- ContT . allocaArray $ length strs
	pcstra <$ lift (pokeArray pcstra cstrl)

textListToCStringArray :: [Txt.Text] -> ContT r IO (Ptr CString)
textListToCStringArray txts = do
	cstrl <- textToCString `mapM` txts
	pcstra <- ContT . allocaArray $ length txts
	pcstra <$ lift (pokeArray pcstra cstrl)

type PtrVoid = Ptr ()

type PtrCString = Ptr CString

success :: #{type VkResult}
success = #{const VK_SUCCESS}

type PtrResult = Ptr #{type VkResult}

vkFalse, vkTrue :: #{type VkBool32}
vkFalse = #{const VK_FALSE}
vkTrue = #{const VK_TRUE}

pattern NullHandle :: Ptr a
pattern NullHandle <- (ptrToWordPtr -> (WordPtr #{const VK_NULL_HANDLE})) where
	NullHandle = wordPtrToPtr $ WordPtr #{const VK_NULL_HANDLE}

queueGraphicsBit :: #{type VkQueueFlags}
queueGraphicsBit = #{const VK_QUEUE_GRAPHICS_BIT}

type PtrFloat = Ptr #{type float}

uint32Max :: #{type uint32_t}
uint32Max = #{const UINT32_MAX}

type PtrUint32T = Ptr #{type uint32_t}
type ListUint32T = [#{type uint32_t}]

uint64Max :: #{type uint64_t}
uint64Max = #{const UINT64_MAX}

textToCString :: Txt.Text -> ContT r IO CString
textToCString t = do
	(cs, ln) <- ContT $ Txt.withCStringLen t
	cs' <- ContT . allocaArray $ ln + 1
	cs' <$ lift do
		copyBytes cs' cs ln
		poke (cs' `plusPtr` ln :: Ptr CChar) 0

pokeText :: Int -> CString -> Txt.Text -> IO ()
pokeText mx dst t = ($ pure) $ runContT do
	(src, ln) <- ContT $ Txt.withCStringLen t
	let	ln' = min ln (mx - 1)
	lift do	copyBytes dst src ln'
		poke (dst `plusPtr` ln' :: Ptr CChar) 0

cstringToText :: CString -> IO Txt.Text
cstringToText cs = Txt.peekCStringLen =<< cstringToCStringLen cs

cstringLength :: CString -> IO Int
cstringLength pc = do
	c <- peek pc
	case c of
		0 -> pure 0
		_ -> (+ 1) <$> cstringLength (pc `plusPtr` 1)

cstringToCStringLen :: CString -> IO CStringLen
cstringToCStringLen cs = (cs ,) <$> cstringLength cs

sTypeCheck :: #{type VkStructureType} -> Ptr a -> IO ()
sTypeCheck st p = do
	st' <- peek $ castPtr p
	when (st /= st') $ error "Vulkan Structure Type not match"

type ListFloat = [#{type float}]

boolToBool32 :: Bool -> #{type VkBool32}
boolToBool32 False = #{const VK_FALSE}
boolToBool32 True = #{const VK_TRUE}

bool32ToBool :: #{type VkBool32} -> Bool
bool32ToBool #{const VK_FALSE} = False
bool32ToBool #{const VK_TRUE} = True
bool32ToBool _ = error $
	"Application must not pass any other values than " ++
	"VK_TRUE or VK_FALSE into a Gpu.Vulkan implementation " ++
	"where a VkBool32 is expected"

nil :: Maybe (t ())
nil = Nothing

allocaAndPokeArray :: Storable a => [a] -> ContT r IO (Int, Ptr a)
allocaAndPokeArray (length &&& id -> (xc, xs)) = do
	p <- ContT $ allocaArray xc
	(xc, p) <$ lift (pokeArray p xs)
