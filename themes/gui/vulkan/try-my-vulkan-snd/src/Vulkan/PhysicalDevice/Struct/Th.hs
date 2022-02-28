{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.PhysicalDevice.Struct.Th where

import Language.Haskell.TH
import Control.Arrow
import Data.Maybe
import Data.Word
import Data.Char

import Paths_try_my_vulkan_snd

vkPhysicalDeviceLimits :: DecsQ
vkPhysicalDeviceLimits = (: []) <$> do
	ds <- runIO readStructData
	dataD (cxt []) (mkName "Limits") [] Nothing [recC (mkName "Limits")
		(uncurry member <$> ds)] []

dict :: [(String, TypeQ)]
dict = [
	("uint32_t", conT ''Word32)
	]

noBang :: BangQ
noBang = bang noSourceUnpackedness noSourceStrictness

member :: String -> FieldName -> VarBangTypeQ
member tp_ fn = varBangType (mkName nm) $ bangType noBang tp
	where (nm, tp) = getNameType tp_ fn

getNameType :: String -> FieldName -> (String, TypeQ)
getNameType tp (Atom fn) = ("limits" ++ capitalize fn, fromJust $ lookup tp dict)
getNameType _ _ = error "bad"

capitalize :: String -> String
capitalize "" = ""
capitalize (c : cs) = toUpper c : cs

readStructData :: IO [(String, FieldName)]
readStructData = map ((id *** readName) . (separate '|')) . lines <$>
	(readFile =<< getDataFileName "th/vkPhysicalDeviceLimits.txt")

data FieldName = Atom String | List String Int deriving Show

readName :: String -> FieldName
readName ('A' : ' ' : nm) = Atom nm
readName _ = error "bad"

separate :: Eq a => a -> [a] -> ([a], [a])
separate c str = case span (/= c) str of
	(pre, _ : pst) -> (pre, pst)
	_ -> error "no separater"
