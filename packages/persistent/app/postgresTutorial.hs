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
import Database.Persist.Postgresql

import qualified Data.ByteString as BS

import Data.Time

type Point = (Double, Double)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
Weather
	city	String
	temp_lo	Int
	temp_hi	Int
	prcp	Double Maybe
	date	Day
	deriving Show

Cities
	name	String
	location Point
	deriving Show
|]

showWeather :: Entity Weather -> String
showWeather w = unwords [wc, show wtl, show wth, show wp, show wd]
	where
	Weather {
		weatherCity = wc,
		weatherTemp_lo = wtl,
		weatherTemp_hi = wth,
		weatherPrcp = wp,
		weatherDate = wd } = entityVal w

showCities :: Entity Cities -> String
showCities c = unwords [cn, show cl]
	where
	Cities {
		citiesName = cn,
		citiesLocation = cl } = entityVal c

database :: BS.ByteString
database = "host=localhost port=5432 user=tatsuya dbname=mydb"

main :: IO ()
main = runNoLoggingT $ runResourceT $ withPostgresqlConn database $ runSqlConn $ do
	runMigration migrateAll

	_ <- insert . Weather
		"San Francisco" 46 50 (Just 0.25) $ fromGregorian 1994 11 27
	_ <- insert $ Cities "San Francisco" (-194.0, 53.0)
	_ <- insert . Weather
		"San Francisco" 43 57 (Just 0.0) $ fromGregorian 1994 11 29
	_ <- insert . Weather "Hayward" 54 37 Nothing $ fromGregorian 1994 11 29

	selectList ([] :: [Filter Weather]) []
		>>= liftIO . putStr . unlines . map showWeather
	liftIO $ putStrLn ""
	selectList ([] :: [Filter Cities]) []
		>>= liftIO . putStr . unlines . map showCities
	liftIO $ putStrLn ""
	selectList [	WeatherCity ==. "San Francisco",
			Filter WeatherPrcp (Left $ Just 0) Gt
			] []
		>>= liftIO . putStr . unlines . map showWeather
	liftIO $ putStrLn ""
	liftIO $ putStrLn ""
	selectList [] [Asc WeatherCity, Asc WeatherTemp_lo]
		>>= liftIO . putStr . unlines . map showWeather
	liftIO $ putStrLn ""

	deleteWhere ([] :: [Filter Weather])
	deleteWhere ([] :: [Filter Cities])

--	johnId <- insert $ Weather "John Doe" $ Just 35
--	janeId <- insert $ Weather "Jane Doe" $ Nothing

--	_ <- insert $ BlogPost "My fr1st p0st" johnId
--	_ <- insert $ BlogPost "One more for good measure" johnId

--	oneJohnPost <- selectList [BlogPostAuthorId ==. johnId] [LimitTo 2]
--	liftIO $ print $ map (blogPostTitle . entityVal) oneJohnPost

--	john <- get johnId
--	liftIO $ print $ fmap weatherName john

--	delete janeId
--	deleteWhere [BlogPostAuthorId ==. johnId]

	return ()
