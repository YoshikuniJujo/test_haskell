{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad.Trans.Resource
import Control.Monad.Logger
import Control.Monad.IO.Class

import Database.Persist
import Database.Persist.TH
import Database.Persist.Sql
import Database.Persist.Sqlite

import qualified Data.Text as Text

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
Person
	name	String
	age	Int Maybe
BlogPost
	title	String
	authorId PersonId
|]

database :: Text.Text
database = ":memory:"

main :: IO ()
main = runNoLoggingT $ runResourceT $ withSqliteConn database $ runSqlConn $ do
	runMigration migrateAll

	johnId <- insert $ Person "John Doe" $ Just 35
	janeId <- insert $ Person "Jane Doe" $ Nothing

	_ <- insert $ BlogPost "My fr1st p0st" johnId
	_ <- insert $ BlogPost "One more for good measure" johnId

	oneJohnPost <- selectList [BlogPostAuthorId ==. johnId] [LimitTo 2]
	liftIO $ print $ map (blogPostTitle . entityVal) oneJohnPost

	john <- get johnId
	liftIO $ print $ fmap personName john

	delete janeId
	deleteWhere [BlogPostAuthorId ==. johnId]

	return ()
