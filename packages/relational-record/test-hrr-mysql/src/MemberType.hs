{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MemberType (
	members, showMembers,
	memberTypeOperations, showMemberTypeOperations,
	showNewestMemberTypeOperation,
	updateMemberType ) where

import Data.Functor.ProductIsomorphic
import Data.Int
import Data.Time
import Database.Relational

import Members as Mbs
import MemberTypeOperations as Mto

updateMemberType :: ZonedTime -> Update ()
updateMemberType zt = updateNoPH $ \proj -> do
	mtos <- queryScalar . unsafeUnique . relation $ do
		nmto <- query $ newestMemberTypeOperation zt
		wheres $ nmto ! Mto.memberId' .=. proj ! Mbs.id'
		return $ nmto ! Mto.memberTypeId'
	mtol <- queryList $ relation $ do
		nmto <- query $ newestMemberTypeOperation zt
		wheres $ nmto ! Mto.memberId' .=. proj ! Mbs.id'
		return $ value (1 :: Int32)
	Mbs.memberTypeId' <-# fromMaybe (value 0) mtos
	wheres $ exists mtol

{-
showNewestOperation :: ZonedTime -> Relation () (Int32, Maybe LocalTime)
showNewestOperation zt = relation $ do
	nwop <- query $ newestOperation zt
	return $ nwop ! newOpMemberId' >< nwop ! newOpCreatedAt'
	-}

showNewestMemberTypeOperation ::
	ZonedTime -> Relation () (Int32, Int32, Int32, LocalTime)
showNewestMemberTypeOperation zt = relation $ do
	nmto <- query $ newestMemberTypeOperation zt
	return $ (,,,)
		|$| nmto ! Mto.id'
		|*| nmto ! memberId'
		|*| nmto ! Mto.memberTypeId'
		|*| nmto ! createdAt'

newestMemberTypeOperation ::
	ZonedTime -> Relation () MemberTypeOperations
newestMemberTypeOperation zt = relation $ do
	mto <- query memberTypeOperations
	nwop <- query $ newestOperation zt
	on $ mto ! memberId' .=. nwop ! newOpMemberId'
	on $ just (mto ! createdAt') .=. nwop ! newOpCreatedAt'
	return $ MemberTypeOperations
		|$| mto ! Mto.id'
		|*| mto ! memberId'
		|*| mto ! Mto.memberTypeId'
		|*| mto ! createdAt'
