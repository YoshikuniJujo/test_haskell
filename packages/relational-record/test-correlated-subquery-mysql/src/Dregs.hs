{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Dregs where

import Data.Functor.ProductIsomorphic
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
--		p <- query persons
--		wheres $ m ! Mny.pid' .=. p ! Psn.id'
		wheres $ m ! Mny.pid' .=. proj ! Psn.id'
		wheres $ m ! Mny.money' .>=. value 100
		return (value (1 :: Int32))
	return unitPlaceHolder

some :: Relation () Int32
some = relation $ subquery =<< query persons

other :: Update ()
other = updateNoPH $ \(proj :: Record Flat Persons) -> do
	canBuy' <-# value 123
	wheres . exists =<< queryList (relation $ subquery proj)
--	wheres . exists =<< queryList (relation $ subquery =<< query persons)

subquery :: Record Flat Persons -> QuerySimple (Record Flat Int32)
subquery p = do
--		p <- query persons
		m <- query monies
		wheres $ m ! Mny.pid' .=. p ! Psn.id'
		wheres $ m ! Mny.money' .>=. value 100
		return (value (1 :: Int32))

subquery2 :: QuerySimple (Record Flat Int32)
subquery2 = do
		p <- query persons
		m <- query monies
		wheres $ m ! Mny.pid' .=. p ! Psn.id'
		wheres $ m ! Mny.money' .>=. value 100
		return (value (1 :: Int32))

updatePersonByMoney2 :: Update ()
updatePersonByMoney2 = update $ \proj -> do
	canBuy' <-# value (1 :: Int32)
	(wheres . exists =<<) . queryList . relation $ do
		m <- query monies2
		wheres $ m ! Mny2.pid' .=. proj ! Psn.id'
		wheres $ m ! Mny2.money' .>=. value 100
		return (value (1 :: Int32))
	return unitPlaceHolder

showPersons :: Relation () (Int32, Int32)
showPersons = relation $ do
	p <- query persons
	return $ (,) |$| p ! Psn.id' |*| p ! Psn.canBuy'

resetPersons :: Update ()
resetPersons = updateNoPH $ \_ -> canBuy' <-# value 0
