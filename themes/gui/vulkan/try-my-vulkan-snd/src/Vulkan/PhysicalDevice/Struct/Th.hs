{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.PhysicalDevice.Struct.Th (
	vkPhysicalDeviceLimits, vkPhysicalDeviceFeatures,
	DeviceSize(..), Size(..) ) where

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

newtype DeviceSize = DeviceSize { unDeviceSize :: Word64 }
	deriving Show

newtype Size = Size Word64 deriving Show

vkPhysicalDeviceLimits :: DecsQ
vkPhysicalDeviceLimits = makeData "Limits"

vkPhysicalDeviceFeatures :: DecsQ
vkPhysicalDeviceFeatures = makeData "Features"

makeData :: String -> DecsQ
makeData dtnm = (\dt sg bd -> [dt, sg, bd])
	<$> vkPhysicalDeviceData dtnm
	<*> vkPhysicalDeviceFunSig dtnm
	<*> vkPhysicalDeviceFunBody dtnm

vkPhysicalDeviceData :: String -> DecQ
vkPhysicalDeviceData dtnm = do
	ds <- runIO $ readStructData dtnm
	dataD (cxt []) (mkName dtnm) [] Nothing [recC (mkName dtnm)
		(uncurry (member dtnm) <$> ds)] [derivClause Nothing [conT ''Show]]

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

member :: String -> String -> FieldName -> VarBangTypeQ
member dtnm tp_ fn = varBangType (mkName nm) $ bangType noBang tp
	where
	pfx = map toLower dtnm
	(nm, tp) = getNameType pfx tp_ fn

getNameType :: String -> String -> FieldName -> (String, TypeQ)
getNameType pfx tp (Atom fn) = (pfx ++ capitalize fn, fst $ lookup' tp dict)
getNameType pfx tp (List fn nb) = (pfx ++ capitalize fn,
	conT ''LengthL `appT` litT (numTyLit nb) `appT` fst (lookup' tp dict))

lookup' :: (Show a, Eq a) => a -> [(a, b)] -> b
lookup' x d = case lookup x d of
	Nothing -> error $ "no such key: " ++ show x
	Just y -> y

capitalize :: String -> String
capitalize "" = ""
capitalize (c : cs) = toUpper c : cs

readStructData :: String -> IO [(String, FieldName)]
readStructData dtnm = map ((id *** readName) . (separate '|')) . lines <$>
	(readFile $ "th/vkPhysicalDevice" ++ dtnm ++ ".txt")

data FieldName = Atom String | List String Integer deriving Show

readName :: String -> FieldName
readName ('A' : ' ' : nm) = Atom nm
readName ('L' : ' ' : nmnb) = let [nm, nb] = words nmnb in List nm (read nb)
readName _ = error "bad"

separate :: Eq a => a -> [a] -> ([a], [a])
separate c str = case span (/= c) str of
	(pre, _ : pst) -> (pre, pst)
	_ -> error "no separater"

vkPhysicalDeviceFunSig :: String -> DecQ
vkPhysicalDeviceFunSig dtnm = sigD (mkName $ map toLower dtnm ++ "FromCore")
	$ (conT =<< (fromJust <$> lookupTypeName ("C." ++ dtnm))) `arrT` conT (mkName dtnm)

arrT :: TypeQ -> TypeQ -> TypeQ
arrT t1 t2 = arrowT `appT` t1 `appT` t2

vkPhysicalDeviceFunBody :: String -> DecQ
vkPhysicalDeviceFunBody dtnm = do
	ds <- runIO $ readStructData dtnm
	xs <- replicateM (length ds) $ newName "x"
	let	fs = (\(tp, _) -> typeToFun tp) <$> ds
		nvs = zip (map snd ds) xs
		nws = zip (zip (map snd ds) xs) fs
	funD (mkName $ map toLower dtnm ++ "FromCore") [
		clause [(`recP` exFieldPats dtnm nvs) =<< fromJust <$> lookupValueName ("C." ++ dtnm)]
			(normalB $ recConE (mkName dtnm) (exFieldExps dtnm nws)) []
		]

typeToFun :: String -> (Name -> ExpQ)
typeToFun nm = let Just (_, f) = lookup nm dict in f

exFieldPats :: String -> [(FieldName, Name)] -> [Q FieldPat]
exFieldPats dtnm = map $ uncurry (exFieldPat1 $ map toLower dtnm)

exFieldPat1 :: String -> FieldName -> Name -> Q FieldPat
exFieldPat1 pfx fn x = do
	let	nm = case fn of
			Atom n -> n
			List n _ -> n
	n <- fromJust <$> lookupValueName ("C." ++ pfx ++ capitalize nm)
	fieldPat n (varP x)

exFieldExps :: String -> [((FieldName, Name), Name -> ExpQ)] -> [Q (Name, Exp)]
exFieldExps dtnm = map . uncurry $ uncurry (exFieldExp1 $ map toLower dtnm)

listToLengthL :: ListToLengthL n => [a] -> LengthL n a
listToLengthL xs = case splitL xs of
	Right (ln, []) -> ln
	_ -> error "bad"

exFieldExp1 :: String -> FieldName -> Name -> (Name -> ExpQ) -> Q (Name, Exp)
exFieldExp1 pfx (Atom nm) x f = fieldExp (mkName $ pfx ++ capitalize nm) (f x)
exFieldExp1 pfx (List nm _) x f = do
	y <- newName "y"
	fieldExp (mkName $ pfx ++ capitalize nm)
		. appE (varE 'listToLengthL) $ lam1E (varP y) (f y) .<$> varE x

(.<$>) :: ExpQ -> ExpQ -> ExpQ
f .<$> x = infixApp f (varE '(<$>)) x
