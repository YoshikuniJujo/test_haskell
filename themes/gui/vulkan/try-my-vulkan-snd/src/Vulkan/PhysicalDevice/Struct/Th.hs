{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.PhysicalDevice.Struct.Th where

import Language.Haskell.TH
import Control.Arrow
import Control.Monad
import Data.Maybe
import Data.Word
import Data.Char

import Paths_try_my_vulkan_snd

import qualified Vulkan.PhysicalDevice.Struct.Core as C

vkPhysicalDeviceLimits :: DecsQ
vkPhysicalDeviceLimits = (\dt sg bd -> [dt, sg, bd])
	<$> vkPhysicalDeviceLimitsData
	<*> vkPhysicalDeviceLimitsFunSig
	<*> vkPhysicalDeviceLimitsFunBody

vkPhysicalDeviceLimitsData :: DecQ
vkPhysicalDeviceLimitsData = do
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

vkPhysicalDeviceLimitsFunSig :: DecQ
vkPhysicalDeviceLimitsFunSig = sigD (mkName "limitsFromCore")
	$ conT (mkName "Limits") `arrT` conT ''C.Limits

arrT :: TypeQ -> TypeQ -> TypeQ
arrT t1 t2 = arrowT `appT` t1 `appT` t2

vkPhysicalDeviceLimitsFunBody :: DecQ
vkPhysicalDeviceLimitsFunBody = do
	ds <- runIO readStructData
	xs <- replicateM (length ds) $ newName "x"
	let	nvs = zip (map ((\(Atom nm) -> "limits" ++ capitalize nm) . snd) ds) xs
	funD (mkName "limitsFromCore") [
		clause [recP (mkName "Limits") (exFieldPats nvs)]
			(normalB $ recConE 'C.Limits (exFieldExps nvs)) []
		]

exFieldPats :: [(String, Name)] -> [Q FieldPat]
exFieldPats = map $ uncurry exFieldPat1

exFieldPat1 :: String -> Name -> Q FieldPat
exFieldPat1 nm x = fieldPat (mkName nm) (varP x)

exFieldExps :: [(String, Name)] -> [Q (Name, Exp)]
exFieldExps = map $ uncurry exFieldExp1

exFieldExp1 :: String -> Name -> Q (Name, Exp)
exFieldExp1 nm x = do
	n <- fromJust <$> lookupValueName ("C." ++ nm)
	fieldExp n (varE x)
