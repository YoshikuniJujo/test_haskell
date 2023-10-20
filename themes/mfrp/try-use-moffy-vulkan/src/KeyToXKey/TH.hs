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
mkKeyToXKey = funD (mkName "keyToXKey") $ (<$> keyPairs) \(p, v) ->
	clause [p] (normalB v) []

keyPairs = (++ [(wildP, conE 'Xk_VoidSymbol)]) $ (<$> ['a' .. 'z']) \c -> (
	conP (mkName $ "GlfwG.Ky.Key'" ++ [toUpper c]) [],
	conE . mkName $ "Xk_" ++ [c] )

t1 `arrT` t2 = arrowT `appT` t1 `appT` t2
