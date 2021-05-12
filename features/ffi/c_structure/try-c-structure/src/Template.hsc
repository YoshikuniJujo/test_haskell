{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Template where

import Language.Haskell.TH
import Foreign.ForeignPtr
import Control.Monad
import Data.Bool
import Data.Char
import System.IO.Unsafe

mkNewtype :: String -> DecQ
mkNewtype nt = newtypeD (cxt []) (mkName nt) [] Nothing (normalC (mkName $ nt ++ "_") [
	bangType
		(bang noSourceUnpackedness noSourceStrictness)
		(conT ''ForeignPtr `appT` conT (mkName nt))
	]) []

mkPatternFun :: String -> [(Name, ExpQ)] -> DecsQ
mkPatternFun nt tpks = sequence [
	sigD (mkName $ lcfirst nt) $ conT (mkName nt) `arrT` tupT (conT <$> ts),
	funD (mkName $ lcfirst nt) [do
		ff <- newName "ff"
		pf <- newName . bool "pf" "_" $ null tpks
		clause [conP (mkName $ nt ++ "_") [varP ff]] (
			normalB $ varE 'unsafePerformIO .$
				varE 'withForeignPtr `appE` varE ff
					`appE` lamE [varP pf] (mkPatternFunDo pf pks)
			) []
		] ]
	where (ts, pks) = unzip tpks

mkPatternFunDo :: Name -> [ExpQ] -> ExpQ
mkPatternFunDo pf pks = do
	fs <- replicateM (length pks) $ newName "f"
	doE . (++ [noBindS $ varE 'pure `appE` tupE' (varE <$> fs)]) $ (<$> (fs `zip` pks)) \(f', pk') ->
		bindS (varP f') $ pk' `appE` varE pf

arrT :: TypeQ -> TypeQ -> TypeQ
arrT t1 t2 = arrowT `appT` t1 `appT` t2

tupE' :: [ExpQ] -> ExpQ
tupE' [e] = e
tupE' es = tupE es

tupT :: [TypeQ] -> TypeQ
tupT [t] = t
tupT ts = foldl appT (tupleT $ length ts) ts

infixr 8 .$

(.$) :: ExpQ -> ExpQ -> ExpQ
e1 .$ e2 = infixE (Just e1) (varE '($)) (Just e2)

lcfirst :: String -> String
lcfirst "" = ""
lcfirst (c : cs) = toLower c : cs
