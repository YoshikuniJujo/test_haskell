{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Database.Relational
import Database.HDBC.Record

import Lib
import MemberType

main :: IO ()
main = do
	withConnection $ \conn -> do
		mapM_ (putStrLn . showMembers)
			=<< runQuery conn (relationalQuery members) ()
		putStrLn ""
		mapM_ (putStrLn . showMemberTypeOperations) =<< runQuery
			conn (relationalQuery memberTypeOperations) ()
		putStrLn ""
		print =<< runQuery
			conn
			(relationalQuery . showNewestMemberTypeOperation
				$ read "2019-03-01 00:00:00 JST")
			()

		print =<< runUpdate
			conn
			(updateMemberType $ read "2019-03-01 00:00:00 JST")
			()
		mapM_ (putStrLn . showMembers)
			=<< runQuery conn (relationalQuery members) ()
