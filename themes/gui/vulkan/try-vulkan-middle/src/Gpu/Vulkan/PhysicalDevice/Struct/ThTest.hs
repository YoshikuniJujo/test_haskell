{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.PhysicalDevice.Struct.ThTest where

import Language.Haskell.TH

import Foreign.Ptr
import Foreign.Storable.PeekPoke

import Data.TypeLevel.Maybe qualified as TMaybe
import Gpu.Vulkan.PhysicalDevice.Struct.Core qualified as C

import Gpu.Vulkan.Base.Middle.Internal

import qualified Gpu.Vulkan.Sample.Enum as Sample
import Data.Word
import Data.Int
import Data.List.Length
import Data.Char
import Control.Arrow

mkData :: String -> DecQ
mkData nm = do
	mn <- newName "mn"
	varBangTypes <- getVarBangType "" nm
	dataD (cxt [])
		(mkName nm)
		[plainTV mn] Nothing
		[recC (mkName nm) $
			(varBangType (mkName $ nm' ++ "Next")
				(bangType noBang
					(conT ''TMaybe.M `appT` varT mn))) :
			(drop 2 varBangTypes)] []
	where nm' = appHead toLower nm

mkDataShow :: String -> DecQ
mkDataShow nm = standaloneDerivD
	(cxt [conT ''Show `appT` (conT ''TMaybe.M `appT` varT (mkName "mn"))])
	(conT ''Show `appT` (conT (mkName nm) `appT` varT (mkName "mn")))

mkDataNoNext :: String -> DecQ
mkDataNoNext nm = do
	varBangTypes <- getVarBangType "NoNext" nm
	dataD (cxt []) (mkName nmnnx) [] Nothing
		[recC (mkName nmnnx) $ drop 2 varBangTypes]
		[derivClause Nothing [conT ''Show]]
	where nmnnx = nm ++ "NoNext"

newtype DeviceSize = DeviceSize { unDeviceSize :: Word64 }
	deriving Show

newtype Size = Size Word64 deriving Show

data DescriptorIndexingFeatures mn = DescriptorIndexingFeatures {
	descriptorIndexingFeaturesNext :: TMaybe.M mn,
	descriptorIndexingFeaturesShaderInputAttachmentArrayDynamicIndexing :: Bool
	}

sample :: String
sample = "DescriptorIndexingFeatures"

data DescriptorIndexingFeaturesNoNext = DescriptorIndexingFeaturesNoNext {
	descriptorIndexingFeaturesNoNextShaderInputAttachmentArrayDynamicIndexing :: Bool
	}
	deriving Show

descriptorIndexingFeaturesToCore :: WithPoked (TMaybe.M mn) =>
	DescriptorIndexingFeatures mn ->
	(C.DescriptorIndexingFeatures -> IO a) -> IO ()
descriptorIndexingFeaturesToCore DescriptorIndexingFeatures {
	descriptorIndexingFeaturesNext = mnxt,
	descriptorIndexingFeaturesShaderInputAttachmentArrayDynamicIndexing =
		siaad } f =
	withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
	f C.DescriptorIndexingFeatures {
		C.descriptorIndexingFeaturesPNext = pnxt',
		C.descriptorIndexingFeaturesShaderInputAttachmentArrayDynamicIndexing =
			boolToBool32 siaad }

bar :: DecsQ
bar = [d|
	descriptorIndexingFeaturesToCore :: WithPoked (TMaybe.M mn) =>
		DescriptorIndexingFeatures mn ->
		(C.DescriptorIndexingFeatures -> IO a) -> IO ()
	descriptorIndexingFeaturesToCore DescriptorIndexingFeatures {
		descriptorIndexingFeaturesNext = mnxt,
		descriptorIndexingFeaturesShaderInputAttachmentArrayDynamicIndexing =
			siaad } f =
		withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
		f C.DescriptorIndexingFeatures {
			C.descriptorIndexingFeaturesPNext = pnxt',
			C.descriptorIndexingFeaturesShaderInputAttachmentArrayDynamicIndexing =
				boolToBool32 siaad }
	|]

mkToCoreType :: DecQ
mkToCoreType = sigD (mkName "descriptorIndexingFeatureToCore")
	(forallT []
		(cxt [conT ''WithPoked `appT`
			(conT ''TMaybe.M `appT` varT (mkName "mn"))])
		(conT (mkName "DescriptorIndexingFeatures") `appT` varT (mkName "mn") `arrT`
			(conT ''C.DescriptorIndexingFeatures `arrT` conT ''IO `appT` conT ''()) `arrT`
			conT ''IO `appT` conT ''()))

mkToCoreBody :: DecQ
mkToCoreBody = funD (mkName "descriptorIndexingFeaturesToCore")
	. (: []) . ($ []) . clause [mkToCorePat, varP $ mkName "f"] . normalB
	$ varE 'withPoked' `appE` varE (mkName "mnxt") `appE`
		lamE [varP $ mkName "pnxt"] (
			varE 'withPtrS `appE` varE (mkName "pnxt") `appE`
			lamE [viewP (varE 'castPtr) (varP $ mkName "pnxt'")] (
				varE (mkName "f") `appE`
				recConE 'C.DescriptorIndexingFeatures [
					('C.descriptorIndexingFeaturesPNext ,) <$> varE (mkName "pnxt'"),
					('C.descriptorIndexingFeaturesShaderInputAttachmentArrayDynamicIndexing ,)
						<$> varE 'boolToBool32 `appE` varE (mkName "siaad")
					] ) )

mkToCorePat :: PatQ
mkToCorePat = recP (mkName "DescriptorIndexingFeatures") [
	fieldPat 'descriptorIndexingFeaturesNext (varP $ mkName "mnxt"),
	fieldPat 'descriptorIndexingFeaturesShaderInputAttachmentArrayDynamicIndexing
		(varP $ mkName "siaad")
	]

infixr 8 `arrT`

arrT :: TypeQ -> TypeQ -> TypeQ
t1 `arrT` t2 = arrowT `appT` t1 `appT` t2

descriptorIndexingFeaturesFromCore ::
	C.DescriptorIndexingFeatures -> DescriptorIndexingFeaturesNoNext
descriptorIndexingFeaturesFromCore C.DescriptorIndexingFeatures {
	C.descriptorIndexingFeaturesShaderInputAttachmentArrayDynamicIndexing =
		siaad } =
	DescriptorIndexingFeaturesNoNext {
		descriptorIndexingFeaturesNoNextShaderInputAttachmentArrayDynamicIndexing =
			bool32ToBool siaad }

fromCore :: DecsQ
fromCore = [d|
	descriptorIndexingFeaturesFromCore ::
		C.DescriptorIndexingFeatures -> DescriptorIndexingFeaturesNoNext
	descriptorIndexingFeaturesFromCore C.DescriptorIndexingFeatures {
		C.descriptorIndexingFeaturesShaderInputAttachmentArrayDynamicIndexing =
			siaad } =
		DescriptorIndexingFeaturesNoNext {
			descriptorIndexingFeaturesNoNextShaderInputAttachmentArrayDynamicIndexing =
				bool32ToBool siaad }
	|]

mkFromCoreType :: DecQ
mkFromCoreType = sigD (mkName "descriptorIndexingFeaturesFromCore")
	(conT 'C.DescriptorIndexingFeatures `arrT`
		varT 'DescriptorIndexingFeaturesNoNext)

mkFromCoreBody :: DecQ
mkFromCoreBody = funD (mkName "descriptorIndexingFeaturesFromCore") . (: []) . ($ [])
	. clause [mkFromCorePat] . normalB $ recConE 'DescriptorIndexingFeaturesNoNext [
		('descriptorIndexingFeaturesNoNextShaderInputAttachmentArrayDynamicIndexing ,)
			<$> varE 'bool32ToBool `appE` varE (mkName "siaad")
		]

mkFromCorePat :: PatQ
mkFromCorePat = recP 'C.DescriptorIndexingFeatures [
	fieldPat 'C.descriptorIndexingFeaturesShaderInputAttachmentArrayDynamicIndexing
		(varP $ mkName "siaad") ]

data FieldName = Atom String | List String Integer deriving Show

member :: String -> String -> FieldName -> VarBangTypeQ
member dtnm tp_ fn = varBangType (mkName nm) $ bangType noBang tp
	where
	pfx = appHead toLower dtnm
	(nm, tp) = getNameType pfx tp_ fn

appHead :: (a -> a) -> [a] -> [a]
appHead f = \case [] -> []; x : xs -> f x : xs

getNameType :: String -> String -> FieldName -> (String, TypeQ)
getNameType pfx tp (Atom fn) = (pfx ++ capitalize fn, fst $ lookup' tp dict)
getNameType pfx tp (List fn nb) = (pfx ++ capitalize fn,
	conT ''LengthL `appT` litT (numTyLit nb) `appT` fst (lookup' tp dict))

lookup' :: (Show a, Eq a) => a -> [(a, b)] -> b
lookup' x d = case lookup x d of
	Nothing -> error $ "no such key: " ++ show x
	Just y -> y

dict :: Dict
dict = dictGenToDict dictGen

dictGenToDict :: DictGen -> Dict
dictGenToDict = map \(tp, tfr, _to) -> (tp, tfr)

dict2 :: Dict2
dict2 = dictGenToDict2 dictGen

dictGenToDict2 :: DictGen -> Dict2
dictGenToDict2 = map \(tp, _tfr, to) -> (tp, to)

type Dict = [(String, (TypeQ, Name -> ExpQ))]
type Dict2 = [(String, (Name -> PatQ, Name -> ExpQ))]
type DictGen = [(String, (TypeQ, Name -> ExpQ), (Name -> PatQ, Name -> ExpQ))]

dictGen :: [(String, (TypeQ, Name -> ExpQ), (Name -> PatQ, Name -> ExpQ))]
dictGen = [
	("uint32_t", (conT ''Word32, varE), (varP, varE)),
	("int32_t", (conT ''Int32, varE), (varP, varE)),
	("float", (conT ''Float, varE), (varP, varE)),
	("VkBool32", (conT ''Bool, appE (varE 'bool32ToBool) . varE),
		(varP, appE (varE 'boolToBool32) . varE)),
	("size_t", (conT ''Size, appE (conE 'Size) . varE),
		(conP 'Size . (: []) . varP, varE)),
	("VkDeviceSize", (conT ''DeviceSize, appE (conE 'DeviceSize) . varE),
		(conP 'DeviceSize . (: []) . varP, varE)),
	("VkSampleCountFlags",
		(conT ''Sample.CountFlags,
			appE (conE 'Sample.CountFlagBits) . varE),
		(conP 'Sample.CountFlagBits . (: []) . varP, varE)) ]

capitalize :: String -> String
capitalize "" = ""
capitalize (c : cs) = toUpper c : cs

noBang :: BangQ
noBang = bang noSourceUnpackedness noSourceStrictness

vkPhysicalDeviceData :: String -> DecQ
vkPhysicalDeviceData dtnm = do
	mms <- getVarBangType "" dtnm
	dataD (cxt []) (mkName dtnm) [] Nothing
		[recC (mkName dtnm) mms]
		[derivClause Nothing [conT ''Show]]

getVarBangType :: String -> String -> Q [VarBangTypeQ]
getVarBangType sfx dtnm = do
	ds <- runIO $ readStructData dtnm
	pure $ uncurry (member $ dtnm ++ sfx) <$> ds

readStructData :: String -> IO [(String, FieldName)]
readStructData dtnm = map ((id *** readName) . (separate '|')) . lines <$>
	(readFile $ "th/vkPhysicalDevice" ++ dtnm ++ ".txt")

readName :: String -> FieldName
readName ('A' : ' ' : nm) = Atom nm
readName ('L' : ' ' : nmnb) = let [nm, nb] = words nmnb in List nm (read nb)
readName _ = error "bad"

separate :: Eq a => a -> [a] -> ([a], [a])
separate c str = case span (/= c) str of
	(pre, _ : pst) -> (pre, pst)
	_ -> error "no separater"
