{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module UseCityAge where

import GHC.Generics

import Data.Functor.ProductIsomorphic
import Data.Int
import Database.Relational
import Database.Relational.TH

import CityAge
import MaxAge

getMaxAgeMembers = relation $ do
	ca <- query cityAge
	mc <- query getMaxAges
	on $ ca ! city' .=. mc ! maCity'
	on $ just (ca ! age') .=. mc ! maAge'
	return $ ca ! name' >< ca ! age' >< ca ! city'

getMaxAges = aggregateRelation $ do
	ca <- query cityAge
	c <- groupBy $ ca ! city'
	return $ MaxAge |$| c |*| max' (ca ! age')
