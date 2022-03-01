{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.PhysicalDevice.Struct.Th (
	vkPhysicalDeviceLimits, DeviceSize(..), Size(..) ) where

import Language.Haskell.TH
import Control.Arrow
import Control.Monad
import Data.Maybe
import Data.List.Length
import Data.Word
import Data.Int
import Data.Char

import Vulkan.Base

import qualified Vulkan.Sample.Enum as Sample
import qualified Vulkan.PhysicalDevice.Struct.Core as C

newtype DeviceSize = DeviceSize { unDeviceSize :: Word64 }
	deriving Show

newtype Size = Size Word64 deriving Show

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
	("int32_t", (conT ''Int32, varE)),
	("float", (conT ''Float, varE)),
	("VkBool32", (conT ''Bool, appE (varE 'bool32ToBool) . varE)),
	("size_t", (conT ''Size, appE (conE 'Size) . varE)),
	("VkDeviceSize", (conT ''DeviceSize, appE (conE 'DeviceSize) . varE)),
	("VkSampleCountFlags", (
		conT ''Sample.CountFlags,
		appE (conE 'Sample.CountFlagBits) . varE) ) ]

noBang :: BangQ
noBang = bang noSourceUnpackedness noSourceStrictness

member :: String -> FieldName -> VarBangTypeQ
member tp_ fn = varBangType (mkName nm) $ bangType noBang tp
	where (nm, tp) = getNameType tp_ fn

getNameType :: String -> FieldName -> (String, TypeQ)
getNameType tp (Atom fn) = ("limits" ++ capitalize fn, fst $ lookup' tp dict)
getNameType tp (List fn nb) = ("limits" ++ capitalize fn,
	conT ''LengthL `appT` litT (numTyLit nb) `appT` fst (lookup' tp dict))

lookup' :: (Show a, Eq a) => a -> [(a, b)] -> b
lookup' x d = case lookup x d of
	Nothing -> error $ "no such key: " ++ show x
	Just y -> y

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
		nvs = zip (map snd ds) xs
		nws = zip (zip (map snd ds) xs) fs
	funD (mkName "limitsFromCore") [
		clause [recP 'C.Limits (exFieldPats nvs)]
			(normalB $ recConE (mkName "Limits") (exFieldExps nws)) []
		]

typeToFun :: String -> (Name -> ExpQ)
typeToFun nm = let Just (_, f) = lookup nm dict in f

exFieldPats :: [(FieldName, Name)] -> [Q FieldPat]
exFieldPats = map $ uncurry exFieldPat1

exFieldPat1 :: FieldName -> Name -> Q FieldPat
exFieldPat1 fn x = do
	let	nm = case fn of
			Atom n -> n
			List n _ -> n
	n <- fromJust <$> lookupValueName ("C.limits" ++ capitalize nm)
	fieldPat n (varP x)

exFieldExps :: [((FieldName, Name), Name -> ExpQ)] -> [Q (Name, Exp)]
exFieldExps = map . uncurry $ uncurry exFieldExp1

listToLengthL :: ListToLengthL n => [a] -> LengthL n a
listToLengthL xs = case splitL xs of
	Right (ln, []) -> ln
	_ -> error "bad"

exFieldExp1 :: FieldName -> Name -> (Name -> ExpQ) -> Q (Name, Exp)
exFieldExp1 (Atom nm) x f = fieldExp (mkName $ "limits" ++ capitalize nm) (f x)
exFieldExp1 (List nm _) x f = do
	y <- newName "y"
	fieldExp (mkName $ "limits" ++ capitalize nm)
		. appE (varE 'listToLengthL) $ lam1E (varP y) (f y) .<$> varE x

(.<$>) :: ExpQ -> ExpQ -> ExpQ
f .<$> x = infixApp f (varE '(<$>)) x
