{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MemberTypeOperations where

import GHC.Generics

import Control.Monad.IO.Class
import Data.Functor.ProductIsomorphic
import Data.Int
import Data.Time
import Database.Relational
import Database.Relational.TH

import Lib

do	[hst, usr, pwd, db] <- liftIO $ lines <$> readFile "db_connect.info"
	Lib.defineTable hst usr pwd db "member_type_operations"

showMemberTypeOperations :: MemberTypeOperations -> String
showMemberTypeOperations MemberTypeOperations {
	MemberTypeOperations.id = _i,
	memberId = mid,
	memberTypeId = mtid,
	createdAt = cat } =
	show mid ++ " " ++ show mtid ++ " " ++ show cat

data NewOperation = NewOperation {
	newOpMemberId :: Int32,
	newOpCreatedAt :: Maybe LocalTime }
	deriving (Show, Generic)

$(makeRelationalRecordDefault ''NewOperation)

newestOperation :: ZonedTime -> Relation () NewOperation
newestOperation zt | timeZoneName (zonedTimeZone zt) /= "JST" =
	error "Time zone should be `JST'"
newestOperation zt = aggregateRelation $ do
	mto <- query memberTypeOperations
	mid <- groupBy $ mto ! memberId'
	wheres $ mto ! createdAt' .<. value (zonedTimeToLocalTime zt)
	return $ NewOperation |$| mid |*| max' (mto ! createdAt')

data MemberTypeOperation2 = MemberTypeOperation2 {
	id2 :: Int32,
	memberId2 :: Int32,
	memberTypeId2 :: Int32,
	createdAt2 :: Int32 }
	deriving (Show, Generic)

$(makeRelationalRecordDefault ''MemberTypeOperation2)
