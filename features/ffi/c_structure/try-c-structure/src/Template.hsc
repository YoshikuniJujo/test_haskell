{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Template where

import Foreign.ForeignPtr
import Language.Haskell.TH
import Data.Char
import System.IO.Unsafe

mkNewtype :: String -> DecQ
mkNewtype nt = newtypeD (cxt []) (mkName nt) [] Nothing (normalC (mkName $ nt ++ "_") [
	bangType
		(bang noSourceUnpackedness noSourceStrictness)
		(conT ''ForeignPtr `appT` conT (mkName nt))
	]) []

mkPatternFun :: String -> Name -> ExpQ -> DecsQ
mkPatternFun nt t pk = sequence [
	sigD (mkName $ lcfirst nt) $ conT (mkName nt) `arrT` conT t,
	funD (mkName $ lcfirst nt) [do
		ff <- newName "ff"
		pf <- newName "pf"
		clause [conP (mkName $ nt ++ "_") [varP ff]] (
			normalB $ varE 'unsafePerformIO .$
				varE 'withForeignPtr `appE` varE ff
					`appE` lamE [varP pf] (pk `appE` varE pf)
			) []
		] ]

arrT :: TypeQ -> TypeQ -> TypeQ
arrT t1 t2 = arrowT `appT` t1 `appT` t2

infixr 8 .$

(.$) :: ExpQ -> ExpQ -> ExpQ
e1 .$ e2 = infixE (Just e1) (varE '($)) (Just e2)

lcfirst :: String -> String
lcfirst "" = ""
lcfirst (c : cs) = toLower c : cs
