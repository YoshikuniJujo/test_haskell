{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Event.Key.Internal (
	-- * Type
	Key(..), foo ) where

import Data.Word (Word64)

import Language.Haskell.TH
import Control.Arrow
import Data.Char

newtype Key = Key Word64 deriving (Show, Eq, Ord)

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
