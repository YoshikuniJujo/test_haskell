{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CEnum.Sample where

import Foreign.C.Enum

enum "EnumSample" ''Int [''Show, ''Read, ''Eq] [
	("EnumSample1", 1),
	("EnumSample2", 2),
	("EnumSample3", 3) ]
