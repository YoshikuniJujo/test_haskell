{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.PhysicalDevice.Struct.Th (vkPhysicalDeviceLimits, DeviceSize(..)) where

import Language.Haskell.TH
import Control.Arrow
import Control.Monad
import Data.Maybe
import Data.List.Length
import Data.Word
import Data.Char

import qualified Vulkan.PhysicalDevice.Struct.Core as C

newtype DeviceSize = DeviceSize { unDeviceSize :: Word64 }
	deriving Show

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

dict :: [(String, (TypeQ, Name -> ExpQ))]
dict = [
	("uint32_t", (conT ''Word32, varE)),
	("VkDeviceSize", (conT ''DeviceSize, appE (conE 'DeviceSize) . varE))
	]

noBang :: BangQ
noBang = bang noSourceUnpackedness noSourceStrictness

member :: String -> FieldName -> VarBangTypeQ
member tp_ fn = varBangType (mkName nm) $ bangType noBang tp
	where (nm, tp) = getNameType tp_ fn

getNameType :: String -> FieldName -> (String, TypeQ)
getNameType tp (Atom fn) = ("limits" ++ capitalize fn, fst . fromJust $ lookup tp dict)
getNameType tp (List fn nb) = ("limits" ++ capitalize fn,
	conT ''LengthR `appT` litT (numTyLit nb) `appT` fst (fromJust $ lookup tp dict))

capitalize :: String -> String
capitalize "" = ""
capitalize (c : cs) = toUpper c : cs

readStructData :: IO [(String, FieldName)]
readStructData = map ((id *** readName) . (separate '|')) . lines <$>
	(readFile "th/vkPhysicalDeviceLimits.txt")

data FieldName = Atom String | List String Integer deriving Show

readName :: String -> FieldName
readName ('A' : ' ' : nm) = Atom nm
readName ('L' : ' ' : nmnb) = let [nm, nb] = words nmnb in List nm (read nb)
readName _ = error "bad"

separate :: Eq a => a -> [a] -> ([a], [a])
separate c str = case span (/= c) str of
	(pre, _ : pst) -> (pre, pst)
	_ -> error "no separater"

vkPhysicalDeviceLimitsFunSig :: DecQ
vkPhysicalDeviceLimitsFunSig = sigD (mkName "limitsFromCore")
	$ conT ''C.Limits `arrT` conT (mkName "Limits")

arrT :: TypeQ -> TypeQ -> TypeQ
arrT t1 t2 = arrowT `appT` t1 `appT` t2

vkPhysicalDeviceLimitsFunBody :: DecQ
vkPhysicalDeviceLimitsFunBody = do
	ds <- runIO readStructData
	xs <- replicateM (length ds) $ newName "x"
	let	fs = (\(tp, _) -> typeToFun tp) <$> ds
		nvs = zip (map ((\(Atom nm) -> "limits" ++ capitalize nm) . snd) ds) xs
		nws = zip (map ((\(Atom nm) -> "limits" ++ capitalize nm) . snd) ds) $ zipWith ($) fs xs
	funD (mkName "limitsFromCore") [
		clause [recP 'C.Limits (exFieldPats nvs)]
			(normalB $ recConE (mkName "Limits") (exFieldExps nws)) []
		]

typeToFun :: String -> (Name -> ExpQ)
typeToFun nm = let Just (_, f) = lookup nm dict in f

exFieldPats :: [(String, Name)] -> [Q FieldPat]
exFieldPats = map $ uncurry exFieldPat1

exFieldPat1 :: String -> Name -> Q FieldPat
exFieldPat1 nm x = do
	n <- fromJust <$> lookupValueName ("C." ++ nm)
	fieldPat n (varP x)

exFieldExps :: [(String, ExpQ)] -> [Q (Name, Exp)]
exFieldExps = map $ uncurry exFieldExp1

exFieldExp1 :: String -> ExpQ -> Q (Name, Exp)
exFieldExp1 nm x = fieldExp (mkName nm) x
