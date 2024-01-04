{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.PhysicalDevice.Struct.Th (
	vkPhysicalDeviceLimits, vkPhysicalDeviceFeatures,
	DeviceSize(..), Size(..),


	vkPhysicalDeviceData, vkPhysicalDeviceFromFunBody
	) where

import Language.Haskell.TH
import Control.Arrow
import Control.Monad
import Data.Foldable
import Data.Maybe
import Data.List.Length
import Data.Word
import Data.Int
import Data.Char

import Gpu.Vulkan.Base.Middle.Internal

import qualified Gpu.Vulkan.Sample.Enum as Sample

newtype DeviceSize = DeviceSize { unDeviceSize :: Word64 }
	deriving Show

newtype Size = Size Word64 deriving Show

vkPhysicalDeviceLimits :: DecsQ
vkPhysicalDeviceLimits = makeData "Limits"

vkPhysicalDeviceFeatures :: DecsQ
vkPhysicalDeviceFeatures = makeData "Features"

makeData :: String -> DecsQ
makeData dtnm = (\dt sg bd tsg tbd -> [dt, sg, bd, tsg, tbd])
	<$> vkPhysicalDeviceData dtnm
	<*> vkPhysicalDeviceFromFunSig dtnm
	<*> vkPhysicalDeviceFromFunBody dtnm
	<*> vkPhysicalDeviceToFunSig dtnm
	<*> vkPhysicalDeviceToFunBody dtnm

vkPhysicalDeviceData :: String -> DecQ
vkPhysicalDeviceData dtnm = do
	ds <- runIO $ readStructData dtnm
	dataD (cxt []) (mkName dtnm) [] Nothing [recC (mkName dtnm)
		(uncurry (member dtnm) <$> ds)] [derivClause Nothing [conT ''Show]]

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

vkPhysicalDeviceFromFunSig :: String -> DecQ
vkPhysicalDeviceFromFunSig dtnm = sigD (mkName $ map toLower dtnm ++ "FromCore")
	$ (conT =<< (fromJustMsg "vkPhysicalDeviceFromFunSig" <$> lookupTypeName ("C." ++ dtnm))) `arrT` conT (mkName dtnm)

vkPhysicalDeviceToFunSig :: String -> DecQ
vkPhysicalDeviceToFunSig dtnm = sigD (mkName $ map toLower dtnm ++ "ToCore")
	$ conT (mkName dtnm) `arrT` (conT =<< (fromJustMsg "vkPhysicalDeviceToFunSig" <$> lookupTypeName ("C." ++ dtnm)))

arrT :: TypeQ -> TypeQ -> TypeQ
arrT t1 t2 = arrowT `appT` t1 `appT` t2

vkPhysicalDeviceFromFunBody :: String -> DecQ
vkPhysicalDeviceFromFunBody dtnm = do
	ds <- runIO $ readStructData dtnm
	xs <- replicateM (length ds) $ newName "x"
	let	fs = (\(tp, _) -> typeToFun tp) <$> ds
		nvs = zip (map snd ds) xs
		nws = zip (zip (map snd ds) xs) fs
	funD (mkName $ map toLower dtnm ++ "FromCore") [
		clause [(`recP` exFieldPats dtnm nvs) =<< fromJustMsg "vkPhysicalDeviceFromFunBody" <$> lookupValueName' ("C." ++ dtnm)]
			(normalB $ recConE (mkName dtnm) (exFieldExps dtnm nws)) []
		]

vkPhysicalDeviceToFunBody :: String -> DecQ
vkPhysicalDeviceToFunBody dtnm = do
	ds <- runIO $ readStructData dtnm
	xs <- replicateM (length ds) $ newName "x"
	let	pes = (\(tp, _) -> typeToPatExp tp) <$> ds
		nvs = zip (zip (map snd ds) xs) pes
	funD (mkName $ map toLower dtnm ++ "ToCore") [
		clause [(recP (mkName dtnm) $ toFieldPats dtnm nvs)]
			(normalB $ (`recConE` toFieldExps dtnm nvs) =<< fromJustMsg "vkPhysicalDeviceToFunBody" <$> lookupValueName' ("C." ++ dtnm)) []]

typeToFun :: String -> (Name -> ExpQ)
typeToFun nm = let Just (_, f) = lookup nm dict in f

exFieldPats :: String -> [(FieldName, Name)] -> [Q FieldPat]
exFieldPats dtnm = map $ uncurry (exFieldPat1 $ map toLower dtnm)

exFieldPat1 :: String -> FieldName -> Name -> Q FieldPat
exFieldPat1 pfx fn x = do
	let	nm = case fn of
			Atom n -> n
			List n _ -> n
	n <- fromJustMsg (mkMsg pfx nm) <$> lookupValueName' ("C." ++ pfx ++ capitalize nm)
	fieldPat n (varP x)
	where
	mkMsg p n = "exFieldPat1: pfx = " ++ show p ++ " nm = " ++ show n

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

typeToPatExp :: String -> (Name -> PatQ, Name -> ExpQ)
typeToPatExp = fromJustMsg "typeToPatExp" . (`lookup` dict2)

toFieldPats :: String -> [((FieldName, Name), (Name -> PatQ, a))] -> [Q FieldPat]
toFieldPats dtnm = map . uncurry $ uncurry (toFieldPat1 $ map toLower dtnm)

toFieldPat1 :: String -> FieldName -> Name -> ((Name -> PatQ), a) -> Q FieldPat
toFieldPat1 pfx (Atom nm) x (f, _) = fieldPat (mkName $ pfx ++ capitalize nm) (f x)
toFieldPat1 pfx (List nm _) x _ = fieldPat (mkName $ pfx ++ capitalize nm) (varP x)

toFieldExps :: String -> [((FieldName, Name), (Name -> PatQ, Name -> ExpQ))] -> [Q (Name, Exp)]
toFieldExps dtnm = map . uncurry $ uncurry (toFieldExp1 $ map toLower dtnm)

toFieldExp1 :: String -> FieldName -> Name -> (Name -> PatQ, Name -> ExpQ) -> Q (Name, Exp)
toFieldExp1 pfx (Atom nm) x (_, f) = do
	n <- fromJustMsg "toFieldExp1" <$> lookupValueName' ("C." ++ pfx ++ capitalize nm)
	fieldExp n (f x)
toFieldExp1 pfx (List nm _) x (pf, f) = do
	n <- fromJustMsg "toFieldExp1" <$> lookupValueName' ("C." ++ pfx ++ capitalize nm)
	y <- newName "y"
	fieldExp n $ lam1E (pf y) (f y) .<$> (varE 'toList `appE` varE x)

fromJustMsg :: String -> Maybe a -> a
fromJustMsg msg Nothing = error msg
fromJustMsg _ (Just x) = x

lookupValueName' :: String -> Q (Maybe Name)
lookupValueName' = pure . Just . mkName
