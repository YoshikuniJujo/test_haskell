{-# LANGUAGE
	TemplateHaskell,
	DataKinds,
	TypeSynonymInstances,
	FlexibleInstances,
	MultiParamTypeClasses,
	DeriveGeneric
	#-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MaxAge where

import GHC.Generics

import Data.Functor.ProductIsomorphic
import Data.Int
import Database.Relational
import Database.Relational.TH

data MaxAge = MaxAge {
	maCity :: String,
	maAge :: Maybe Int32
	} deriving (Show, Generic)

$(makeRelationalRecordDefault ''MaxAge)
