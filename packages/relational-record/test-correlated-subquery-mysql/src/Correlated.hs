{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Correlated where

import Data.Int

import Database.Relational

import Person as Psn
import Money as Mny
import Money2 as Mny2

updatePersonByMoney :: Update ()
updatePersonByMoney = update $ \proj -> do
	canBuy' <-# value (1 :: Int32)
	(wheres . exists =<<) . queryList . relation $ do
		m <- query monies
		wheres $ m ! Mny.pid' .=. proj ! Psn.id'
		wheres $ m ! Mny.money' .>=. value 100
		return (value (1 :: Int32))
	return unitPlaceHolder

updatePersonByMoney2 :: Update ()
updatePersonByMoney2 = update $ \proj -> do
	canBuy' <-# value (1 :: Int32)
	(wheres . exists =<<) . queryList . relation $ do
		m <- query monies2
		wheres $ m ! Mny2.pid' .=. proj ! Psn.id'
		wheres $ m ! Mny2.money' .>=. value 100
		return (value (1 :: Int32))
	return unitPlaceHolder
