{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Event.Key.Internal.TH where

import Language.Haskell.TH
import Control.Arrow
import Data.Char

import Control.Moffy.Event.Key

foo :: Name -> DecsQ
foo nm = sequence [
	patSynSigD (capName nm) $ conT ''Key,
	patSynD (capName nm) (prefixPatSyn []) unidir
		$ viewP (infixE Nothing (varE '(==)) (Just (varE nm))) (conP 'True [])
	]

cap :: String -> String
cap [] = []
cap (h : t) = toUpper h : t

capName :: Name -> Name
capName ((nameModule &&& nameBase) -> (m, b)) = mkName $ cap b
--	mkName . maybe id ((++) . (++ ".")) m $ cap b
