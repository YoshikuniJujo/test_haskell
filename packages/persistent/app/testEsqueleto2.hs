{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Control.Monad.Trans.Resource
import Control.Monad.Logger
import Control.Monad.IO.Class
import Control.Exception

import Database.Persist
import Database.Persist.TH
import Database.Persist.Sql
import Database.Persist.Postgresql

import Database.PostgreSQL.Simple.Internal

import Data.Text (Text)
import Data.Time

import qualified Data.ByteString as BS
import qualified Data.Text as Txt

import qualified Database.Esqueleto as E
import Database.Esqueleto ((^.), (?.))

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Author
	name Text
	deriving Show
Blog
	author AuthorId
	title Text
	content Text
	deriving Show
|]

showAuthor :: Author -> String
showAuthor = Txt.unpack . authorName

showBlog :: Blog -> String
showBlog b =
	show (unSqlBackendKey . unAuthorKey $ blogAuthor b) ++ " " ++
	Txt.unpack (blogTitle b) ++ " " ++
	Txt.unpack (blogContent b)

unValue :: E.Value a -> a
unValue (E.Value x) = x

three :: (a -> a') -> (b -> b') -> (c -> c') -> (a, b, c) -> (a', b', c')
three f g h (x, y, z) = (f x, g y, h z)

database :: BS.ByteString
database = "host=localhost port=5432 user=tatsuya dbname=mydb"

main = main_ `catch` \(e :: SqlError) -> do
	BS.putStrLn $ sqlErrorMsg e
	throwIO e

main_ :: IO ()
main_ = runNoLoggingT $ runResourceT $ withPostgresqlConn database $ runSqlConn $ do
	runMigration migrateAll

	alice <- insert $ Author "Alice"
	bob <- insert $ Author "Bob"

	insert $ Blog alice "Alice's first post" "Hello World!"
	insert $ Blog bob "Bob's first post" "Hello World!"
	insert $ Blog bob "Bob's second post" "Goodbye World!"

	selectList ([] :: [Filter Author]) []
		>>= liftIO . putStr . unlines . map (showAuthor . entityVal)
	liftIO $ putStrLn ""
	selectList ([] :: [Filter Blog]) []
		>>= liftIO . putStr . unlines . map (showBlog . entityVal)

	blogs <- E.select . E.from $ \(blog `E.InnerJoin` author) -> do
		E.on $ blog ^. BlogAuthor E.==. author ^. AuthorId
		return (
			blog	^. BlogId,
			blog	^. BlogTitle,
			author	^. AuthorName )
	liftIO $ putStrLn ""
	liftIO . putStr . unlines
		$ map (show . three
			(unSqlBackendKey . unBlogKey . unValue)
			unValue
			unValue) blogs
	deleteWhere ([] :: [Filter Blog])
	deleteWhere ([] :: [Filter Author])

{-
	johnId <- insert $ Person "john@example.com"
	janeId <- insert $ Person "jane@example.com"
	johnWId <- insert $ Person "john_watson@example.com"

	johnExampleId <- insert $ Handle johnId "example" "John Doe" time
	janeExampleId <- insert $ Handle janeId "example" "Jane Doe" time
	insert $ Handle johnId "irc" "john_doe" time
	johnAnotherId <- insert $ Handle johnWId "another" "John Watson" time

	insert $ Uri johnExampleId "calendar" "http://example.com/calendar/john"
	insert $ Uri johnExampleId "address book" "http://example.com/address/john"
	insert $ Uri johnAnotherId "irc" "http://example.com/calendar/john_watson"

	liftIO $ putStrLn "use normal ==. operator"
	johnHandles <- selectList [HandleName ==. "John Doe"] [LimitTo 1]
	forM_ johnHandles $ \johnHandle -> do
		johnURIs <- selectList [UriHandleId ==. entityKey johnHandle] []
		liftIO $ mapM_ print (johnURIs :: [Entity Uri])
	liftIO $ putStrLn ""
	johnEs <-
		E.select $ E.from $ \(handle `E.LeftOuterJoin` muri) -> do
			E.on $ E.just (handle ^. HandleId) E.==. muri ?. UriHandleId
			E.where_ $ handle ^. HandleName `E.like` (E.%) E.++. E.val "john" E.++. (E.%)
			return (handle, muri)
	liftIO $ forM_ johnEs print

	deleteWhere ([] :: [Filter Uri])
	deleteWhere ([] :: [Filter Handle])
	deleteWhere ([] :: [Filter Person])
-}

	return ()
