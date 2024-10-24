{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module KeyToXKey.TH where

import Language.Haskell.TH
import Data.Char
import Data.KeySym
import Graphics.UI.GlfwG.Key qualified as GlfwG.Ky

mkKeyToXKeyType :: DecQ
mkKeyToXKeyType =
	sigD (mkName "keyToXKey") $ conT ''GlfwG.Ky.Key `arrT` conT ''KeySym

mkKeyToXKey :: DecQ
mkKeyToXKey = funD (mkName "keyToXKey") $
	(\(p, v) -> clause [p] (normalB v) []) <$> keyPairs

keyPairs :: [(Q Pat, Q Exp)]
keyPairs = (++ [(wildP, conE 'Xk_VoidSymbol)]) $ (<$> ['a' .. 'z']) \c -> (
	conP (mkName $ "GlfwG.Ky.Key'" ++ [toUpper c]) [],
	conE . mkName $ "Xk_" ++ [c] )

arrT :: Quote m => m Type -> m Type -> m Type
t `arrT` u = arrowT `appT` t `appT` u
