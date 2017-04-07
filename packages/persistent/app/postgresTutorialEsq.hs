import Template (
	Entity(..), tables, persistLowerCase, runDB, selectAll, deleteAll,
	put, newline )
import Database.Esqueleto (
	InnerJoin(..), (^.), (==.),
	insert, select, from, on, unValue, unSqlBackendKey,
	where_, just, val, (>.), orderBy, asc, distinct, Value(..), max_,
	sub_select, SqlExpr )

import Control.Monad.IO.Class
import Control.Arrow

import Data.List (intercalate, transpose)
import Data.Text (Text)
import qualified Data.Text as Txt (unpack)
import Data.Time
import Point

tables "migrateAll" [persistLowerCase|
Weather
	city		Text
	temp_lo		Int
	temp_hi		Int
	prcp		Double Maybe
	date		Day
	deriving Show
Cities
	name		Text
	location	Point
	deriving Show
|]

main :: IO ()
main = runDB migrateAll $ do

	_ <- insert $ Weather
		"San Francisco" 46 50 (Just 0.25) $ fromGregorian 1994 11 27
	_ <- insert $ Cities
		"San Francisco" $ Point (- 194.0) 53.0
	_ <- insert $ Weather {
		weatherCity = "San Francisco",
		weatherTemp_lo = 43,
		weatherTemp_hi = 57,
		weatherPrcp = Just 0.0,
		weatherDate = fromGregorian 1994 11 29 }
	_ <- insert $ Weather {
		weatherDate = fromGregorian 1994 11 29,
		weatherCity = "Hayward",
		weatherTemp_hi = 54,
		weatherTemp_lo = 37,
		weatherPrcp = Nothing }

	selectAll >>= liftIO . putStr . showWeather
	selectAll >>= liftIO . putStr . showTable
		["city", "temp_avg", "date"]
		[L, R, L] [
			Txt.unpack . weatherCity,
			show . (`div` 2) . uncurry (+)
				. (weatherTemp_lo &&& weatherTemp_hi),
			show . weatherDate ]

	w <- select . from $ \weather -> do
		return weather
	liftIO . putStr $ showWeather w

	w <- select . from $ \weather -> do
		where_ $ weather ^. WeatherPrcp >. just (val 0.0)
		return weather
	liftIO . putStr $ showWeather w

	w <- select . from $ \weather -> do
		orderBy [asc $ weather ^. WeatherCity]
		return weather
	liftIO . putStr $ showWeather w

	w <- select . from $ \weather -> do
		orderBy [
			asc $ weather ^. WeatherCity,
			asc $ weather ^. WeatherTemp_lo ]
		return weather
	liftIO . putStr $ showWeather w

	w <- select . distinct . from $ \weather -> do
		return $ weather ^. WeatherCity
	liftIO $ print (w :: [Value Text])

	w <- select . distinct . from $ \weather -> do
		orderBy [ asc $ weather ^. WeatherCity ]
		return $ weather ^. WeatherCity
	liftIO $ print (w :: [Value Text])

	wc <- select . from $ \(weather `InnerJoin` cities) -> do
		on $ weather ^. WeatherCity ==. cities ^. CitiesName
		return (weather, cities)
	liftIO . putStr $ show2Tables
		["city", "temp_lo", "temp_hi", "prcp", "date",
			"name", "location"]
		[L, R, R, R, R, L, R] [
			Left $ Txt.unpack . weatherCity,
			Left $ show . weatherTemp_lo,
			Left $ show . weatherTemp_hi,
			Left $ maybe "" show . weatherPrcp,
			Left $ show . weatherDate,
			Right $ Txt.unpack . citiesName,
			Right $ show . citiesLocation ]
		(wc :: [(Entity Weather, Entity Cities)])

	tl <- select . from $ \weather -> do
		return $ max_ (weather ^. WeatherTemp_lo)
	liftIO $ print (tl :: [Value (Maybe Int)])

	ct <- select . from $ \weather -> do
		where_ $ ((just $ weather ^. WeatherTemp_lo) ==.)
			$ sub_select . from $ \w -> do
				return $ max_ (w ^. WeatherTemp_lo)
		return $ weather ^. WeatherCity
	liftIO $ print (ct :: [Value Text])

	selectAll @Cities >>= liftIO . mapM_ print
	deleteAll @Weather
	deleteAll @Cities

myJust :: Value a -> Value (Maybe a)
myJust (Value x) = Value $ Just x

showWeather :: [Entity Weather] -> String
showWeather = showTable
	["city", "temp_lo", "temp_hi", "prcp", "date"]
	[L, R, R, R, L] [
		Txt.unpack . weatherCity,
		show . weatherTemp_lo,
		show . weatherTemp_hi,
		maybe "" show . weatherPrcp,
		show . weatherDate ]

showTable :: [String] -> [LeftRight] -> [a -> String] -> [Entity a] -> String
showTable ts lrs fs = (++ "\n") . unlines
	. (\(hl, l1 : ls) ->
		l1 : hl : ls ++ ["(" ++ show (length ls) ++ " rows)"])
	. (mkHorizontalLine &&& map (intercalate " | "))
	. showLines lrs . (ts :) . map (pickup fs)

show2Tables :: [String] -> [LeftRight] ->
	[Either (a -> String) (b -> String)] -> [(Entity a, Entity b)] -> String
show2Tables ts lrs fs = (++ "\n") . unlines
	. (\(hl, l1 : ls) ->
		l1 : hl : ls ++ ["(" ++ show (length ls) ++ " rows)"])
	. (mkHorizontalLine &&& map (intercalate " | "))
	. showLines lrs . (ts :)
	. map (pickup2 fs)

mkHorizontalLine :: [[String]] -> String
mkHorizontalLine (ss : _) = intercalate "-+-"
	$ map (\s -> replicate (length s) '-') ss

showLines :: [LeftRight] -> [[String]] -> [[String]]
showLines lrs = map (zipWith (uncurry . toLen) lrs)
	. uncurry (map . zip $)
	. (map maximum . map (map length) . transpose &&& id)

pickup :: [a -> String] -> Entity a -> [String]
pickup fs x_ = map ($ x) fs
	where
	x = entityVal x_

pickup2 :: [Either (a -> String) (b -> String)] ->
	(Entity a, Entity b) -> [String]
pickup2 fs (x, y) = map (either ($ entityVal x) ($ entityVal y)) fs

data LeftRight = L | R deriving Show

toLen :: LeftRight -> Int -> String -> String
toLen L n s = s ++ replicate (n - length s) ' '
toLen R n s = replicate (n - length s) ' ' ++ s
