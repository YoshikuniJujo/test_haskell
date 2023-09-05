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
-- import Gpu.Vulkan.PhysicalDevice.Struct.Core qualified as C

import Gpu.Vulkan.Base.Middle.Internal

import qualified Gpu.Vulkan.Sample.Enum as Sample
import Data.Word
import Data.Int
import Data.List.Length
import Data.Char
import Control.Arrow
-- import Data.Maybe
import Data.Foldable
import Control.Monad

fromJust' :: String -> Maybe a -> a
fromJust' msg = \case Nothing -> error msg; Just x -> x

makeStructure :: String -> DecsQ
makeStructure nm = do
	dct <- readStructData nm
	sequence [
		mkData nm dct, mkDataShow nm, mkDataNoNext nm dct,
		mkFromCoreType Production nm, mkFromCoreBody Production nm dct,
		mkToCoreType Production nm, mkToCoreBody Production nm dct,
		mkFromNoNextType nm, mkFromNoNextBody nm dct ]

mkData :: String -> DictFieldName -> DecQ
mkData nm dct = do
	mn <- newName "mn"
	let	varBangTypes = getVarBangTypes "" nm dct
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

mkDataNoNext :: String -> DictFieldName -> DecQ
mkDataNoNext nm dct = do
	let	varBangTypes = getVarBangTypes "NoNext" nm dct
	dataD (cxt []) (mkName nmnnx) [] Nothing
		[recC (mkName nmnnx) $ drop 2 varBangTypes]
		[derivClause Nothing [conT ''Show]]
	where nmnnx = nm ++ "NoNext"

getVarBangTypes :: String -> String -> DictFieldName -> [VarBangTypeQ]
getVarBangTypes sfx dtnm ds = uncurry (member $ dtnm ++ sfx) <$> ds

mkFromCoreType :: Debug -> String -> DecQ
mkFromCoreType dbg nm = do
	cnm <- fromJust' "foo" <$> lookupTypeName' dbg ("C." ++ nm)
	sigD (mkName $ nm' ++ "FromCore")
		(conT cnm `arrT` conT (mkName $ nm ++ "NoNext"))
	where nm' = appHead toLower nm

mkFromCoreBody :: Debug -> String -> DictFieldName -> DecQ
mkFromCoreBody dbg nm dct = do
	let	ds = drop 2 dct
	xs <- replicateM (length ds) $ newName "x"
	let	fs = (\(tp, _) -> typeToFun tp) <$> ds
		nvs = zip (map snd ds) xs
		nws = zip (zip (map snd ds) xs) fs
	cnm <- fromJust' "bar" <$> lookupValueName' dbg ("C." ++ nm)
	funD (mkName $ nm' ++ "FromCore") . (: []) . ($ [])
		. clause [mkFromCorePat cnm nvs] . normalB
		$ recConE (mkName $ nm ++ "NoNext") (exFieldExps "NoNext" nm nws)
	where
	nm' = appHead toLower nm
	mkFromCorePat cnm nvs = recP cnm $ exFieldPats dbg nm nvs

mkToCoreType :: Debug -> String -> DecQ
mkToCoreType dbg nm = do
	mn <- newName "mn"
	cnm <- fromJust' "baz" <$> lookupTypeName' dbg ("C." ++ nm)
	sigD (mkName $ nm' ++ "ToCore")
		(forallT []
			(cxt [conT ''WithPoked `appT`
				(conT ''TMaybe.M `appT` varT mn)])
			(conT (mkName nm) `appT` varT mn `arrT`
				(conT cnm `arrT` conT ''IO `appT` conT ''()) `arrT`
				conT ''IO `appT` conT ''()))
	where nm' = appHead toLower nm

mkToCoreBody :: Debug -> String -> DictFieldName -> DecQ
mkToCoreBody dbg nm ds = do
	f <- newName "f"
	cnm <- fromJust' "poooo" <$> lookupValueName' dbg ("C." ++ nm)
	stype <- fromJust' "hogefuga" <$> lookupValueName' dbg ("C." ++ nm' ++ "SType")
	pnext <- fromJust' "oops" <$> lookupValueName' dbg ("C." ++ nm' ++ "PNext")
	xs <- replicateM (length ds) $ newName "x"
	let	pes = (\(tp, _) -> typeToPatExp tp) <$> ds
		nvs = zip (zip (map snd ds) xs) pes
	[mnxt, pnxt, pnxt'] <- newName `mapM` ["mnxt", "pnxt", "pnxt'"]
	funD (mkName $ nm' ++ "ToCore")
		. (: []) . ($ []) . clause [mkToCorePat mnxt nvs, varP f] . normalB
		$ varE 'withPoked' `appE` varE mnxt `appE`
			lamE [varP pnxt] (
				varE 'withPtrS `appE` varE pnxt `appE`
				lamE [viewP (varE 'castPtr) (varP pnxt')] (
					varE f `appE`
					recConE cnm (
						((stype ,) <$> conE '()) : ((pnext ,) <$> varE pnxt') :
						toFieldExps dbg nm (drop 2 nvs) ) ) )
	where
	nm' = appHead toLower nm
	mkToCorePat mnxt nvs = recP (mkName nm)
		$ fieldPat (mkName $ nm' ++ "Next") (varP mnxt) : toFieldPats nm (drop 2 nvs)

type DictFieldName = [(String, FieldName)]

readStructData :: String -> Q DictFieldName
readStructData dtnm = runIO $ map ((id *** readName) . (separate '|')) . lines <$>
	(readFile $ "th/vkPhysicalDevice" ++ dtnm ++ ".txt")

data Debug = Production | Debug deriving Show

lookupValueName' :: Debug -> String -> Q (Maybe Name)
lookupValueName' = \case
	Production -> lookupValueName
	Debug -> pure . Just . mkName

lookupTypeName' :: Debug -> String -> Q (Maybe Name)
lookupTypeName' = \case
	Production -> lookupTypeName
	Debug -> pure . Just . mkName

sample :: String
sample = "DescriptorIndexingFeatures"

newtype DeviceSize = DeviceSize { unDeviceSize :: Word64 } deriving Show
newtype Size = Size Word64 deriving Show

infixr 8 `arrT`

arrT :: TypeQ -> TypeQ -> TypeQ
t1 `arrT` t2 = arrowT `appT` t1 `appT` t2

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

readName :: String -> FieldName
readName ('A' : ' ' : nm) = Atom nm
readName ('L' : ' ' : nmnb) = case words nmnb of
	[nm, nb] -> List nm (read nb); _ -> error "bad"
readName _ = error "bad"

separate :: Eq a => a -> [a] -> ([a], [a])
separate c str = case span (/= c) str of
	(pre, _ : pst) -> (pre, pst)
	_ -> error "no separater"

toFieldExps :: Debug -> String -> [((FieldName, Name), (Name -> PatQ, Name -> ExpQ))] -> [Q (Name, Exp)]
toFieldExps dbg dtnm = map . uncurry $ uncurry (toFieldExp1 dbg $ appHead toLower dtnm)

toFieldExp1 :: Debug -> String -> FieldName -> Name -> (Name -> PatQ, Name -> ExpQ) -> Q (Name, Exp)
toFieldExp1 dbg pfx (Atom nm) x (_, f) = do
	n <- fromJust' "ukki" <$> lookupValueName' dbg ("C." ++ pfx ++ capitalize nm)
	fieldExp n (f x)
toFieldExp1 dbg pfx (List nm _) x (pf, f) = do
	n <- fromJust' "sarusaru" <$> lookupValueName' dbg ("C." ++ pfx ++ capitalize nm)
	y <- newName "y"
	fieldExp n $ lam1E (pf y) (f y) .<$> (varE 'toList `appE` varE x)

typeToFun :: String -> (Name -> ExpQ)
typeToFun nm = case lookup nm dict of Just (_, f) -> f; Nothing -> error "bad"

exFieldPats :: Debug -> String -> [(FieldName, Name)] -> [Q FieldPat]
exFieldPats dbg dtnm = map $ uncurry (exFieldPat1 dbg $ appHead toLower dtnm)

exFieldPat1 :: Debug -> String -> FieldName -> Name -> Q FieldPat
exFieldPat1 dbg pfx fn x = do
	let	nm = case fn of
			Atom n -> n
			List n _ -> n
	n <- fromJust' ("C." ++ pfx ++ capitalize nm) <$> lookupValueName' dbg ("C." ++ pfx ++ capitalize nm)
	fieldPat n (varP x)

typeToPatExp :: String -> (Name -> PatQ, Name -> ExpQ)
typeToPatExp = fromJust' "nande" . (`lookup` dict2)

toFieldPats :: String -> [((FieldName, Name), (Name -> PatQ, a))] -> [Q FieldPat]
toFieldPats dtnm = map . uncurry $ uncurry (toFieldPat1 $ appHead toLower dtnm)

toFieldPat1 :: String -> FieldName -> Name -> ((Name -> PatQ), a) -> Q FieldPat
toFieldPat1 pfx (Atom nm) x (f, _) = fieldPat (mkName $ pfx ++ capitalize nm) (f x)
toFieldPat1 pfx (List nm _) x _ = fieldPat (mkName $ pfx ++ capitalize nm) (varP x)

exFieldExps :: String -> String -> [((FieldName, Name), Name -> ExpQ)] -> [Q (Name, Exp)]
exFieldExps sfx dtnm = map . uncurry $ uncurry (exFieldExp1 sfx $ appHead toLower dtnm)

listToLengthL :: ListToLengthL n => [a] -> LengthL n a
listToLengthL xs = case splitL xs of
	Right (ln, []) -> ln
	_ -> error "bad"

exFieldExp1 :: String -> String -> FieldName -> Name -> (Name -> ExpQ) -> Q (Name, Exp)
exFieldExp1 sfx pfx (Atom nm) x f = fieldExp (mkName $ pfx ++ sfx ++ capitalize nm) (f x)
exFieldExp1 sfx pfx (List nm _) x f = do
	y <- newName "y"
	fieldExp (mkName $ pfx ++ sfx ++ capitalize nm)
		. appE (varE 'listToLengthL) $ lam1E (varP y) (f y) .<$> varE x

(.<$>) :: ExpQ -> ExpQ -> ExpQ
f .<$> x = infixApp f (varE '(<$>)) x

mkFromNoNextType :: String -> DecQ
mkFromNoNextType nm = do
	mn <- newName "mn"
	sigD (mkName $ nm' ++ "FromNoNext") $
		(conT ''TMaybe.M `appT` varT mn) `arrT`
		conT (mkName $ nm ++ "NoNext") `arrT`
		(conT (mkName nm) `appT` varT mn)
	where nm' = appHead toLower nm

mkFromNoNextBody :: String -> DictFieldName -> DecQ
mkFromNoNextBody nm dct = do
	mnxt <- newName "mnxt"
	xs <- replicateM (length dct) $ newName "x"
	funD (mkName $ nm' ++ "FromNoNext") . (: []) . ($ [])
		. clause [varP mnxt, mkFromNoNextPat nm (drop 2 dct) xs] . normalB
		. recConE (mkName nm) $
			((mkName (nm' ++ "Next") ,) <$> varE mnxt) :
			(<$> zip (dictFieldNameToNames $ drop 2 dct) xs) \(fnm, x) ->
				(mkName (nm' ++ capitalize fnm) ,) <$> varE x
	where nm' = appHead toLower nm

mkFromNoNextPat :: String -> DictFieldName -> [Name] -> PatQ
mkFromNoNextPat nm dct xs = recP (mkName $ nm ++ "NoNext")
	$ (<$> zip (dictFieldNameToNames dct) xs) \(fnm, x) ->
		fieldPat (mkName $ nm' ++ "NoNext" ++ capitalize fnm) (varP x)
	where nm' = appHead toLower nm

dictFieldNameToNames :: DictFieldName -> [String]
dictFieldNameToNames = (dfntn <$>)
	where dfntn (_, fn) = case fn of Atom n -> n; List n _ -> n
