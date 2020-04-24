{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Foreign.C.Types
import Data.Time

import Field

type Count = (Day, Double)
type Counts = [Count]

countToPoint ::
	(Day, Day) -> (Position, Position) ->
	(Double, Double) -> (Position, Position) -> Count -> Point
countToPoint (dmn, dmx) (xmn, xmx) (cmn, cmx) (ymn, ymx) (d, c) = point
	(fromIntegral xmn + d `diffDays'` dmn * (fromIntegral $ xmx - xmn) / dmx `diffDays'` dmn)
	(fromIntegral ymn + (c - cmn) * (fromIntegral $ ymx - ymn) / (cmx - cmn))

countToSemilog ::
	(Day, Day) -> (Position, Position) ->
	(Double, Double) -> (Position, Position) -> Count -> Point
countToSemilog (dm, dmx) (xmn, xmx) (cmn, cmx) (ymn, ymx) (d, c) =
	countToPoint (dm, dmx) (xmn, xmx) (log cmn, log cmx) (ymn, ymx) (d, log c)

point :: Double -> Double -> Point
point x y = Point (round x) (round y)

diffDays' :: Day -> Day -> Double
t1 `diffDays'` t2 = fromRational . toRational $ t1 `diffDays` t2

diffUTCTime' :: UTCTime -> UTCTime -> Double
t1 `diffUTCTime'` t2 = fromRational . toRational $ t1 `diffUTCTime` t2

readCounts :: FilePath -> IO Counts
readCounts fp = ((\[dy, i, _d] -> (read dy, read i)) . words <$>) . lines <$> readFile fp

drawGraph :: Field -> Pixel -> CInt -> [Point] -> IO ()
drawGraph f c lw ps = drawLines f c lw ps

waitExposure :: Field -> IO ()
waitExposure f =
	doWhile_ $ withNextEvent f \case
		ExposeEvent {} -> flushField f >> pure False
		_ -> pure True

drawCountsFromFile :: Field -> FilePath -> IO ()
drawCountsFromFile f fp = drawGraph f 0xffffff 2 =<<
	(countToPoint (read "2020-01-01", read "2020-05-01") (100, 500) (0, 18000) (400, 100) <$>) <$> readCounts fp

drawSemilogFromFile :: Field -> FilePath -> IO ()
drawSemilogFromFile f fp = drawGraph f 0xffffff 2 =<<
	(countToSemilog (read "2020-01-01", read "2020-05-01") (100, 500) (1, 18000) (400, 100) <$>) . filter ((/= 0) . snd) <$> readCounts fp

doWhile_ :: Monad m => m Bool -> m ()
doWhile_ act = do
	b <- act
	if b then doWhile_ act else pure ()

close :: Field -> IO ()
close = closeField

mkDiff :: Counts -> Counts
mkDiff [] = []
mkDiff [_] = []
mkDiff ((_d0, c0) : dcs@((d1, c1) : _)) = (d1, c1 - c0) : mkDiff dcs

drawDiffFromFile :: Field -> FilePath -> IO ()
drawDiffFromFile f fp = drawGraph f 0xffffff 2 =<<
	(countToPoint (read "2020-01-01", read "2020-05-01") (100, 500) (0, 1500) (400, 100) <$>) . filter ((/= 0) . snd) . mkDiff <$> readCounts fp

drawDiff :: Field -> Counts -> IO ()
drawDiff f cs = drawGraph f 0x0000ff 5
	$ (countToPoint (read "2020-01-01", read "2020-05-01") (100, 500) (0, 1500) (400, 100) <$>) . filter ((/= 0) . snd) $ mkDiff cs

drawDiffSemilog :: Field -> FilePath -> IO ()
drawDiffSemilog f fp = drawGraph f 0xffffff 2 =<<
	(countToSemilog (read "2020-01-01", read "2020-05-01") (100, 500) (1, 18000) (400, 100) <$>) . filter ((/= 0) . snd) . mkDiff <$> readCounts fp

toWeekly :: Counts -> Counts
toWeekly [] = []
toWeekly (c : cs) = c : toWeekly (drop 6 cs)

drawWeeklyDiffFromFile :: Field -> FilePath -> IO ()
drawWeeklyDiffFromFile f fp = drawGraph f 0xffffff 2 =<<
	(countToPoint (read "2020-01-01", read "2020-05-01") (100, 500) (0, 8000) (400, 100) <$>) . mkDiff . toWeekly <$> readCounts fp

drawWeeklyDiff :: Field -> Counts -> IO ()
drawWeeklyDiff f cs = drawGraph f 0x0000ff 5
	$ (countToPoint (read "2020-01-01", read "2020-05-01") (100, 500) (0, 8000) (400, 100) <$>) . filter ((/= 0) . snd) . mkDiff $ toWeekly cs

drawWeeklyDiffSemilogFromFile :: Field -> FilePath -> IO ()
drawWeeklyDiffSemilogFromFile f fp = drawGraph f 0xffffff 2 =<<
	(countToSemilog (read "2020-01-01", read "2020-05-01") (100, 500) (1, 18000) (400, 100) <$>) . filter ((/= 0) . snd) . mkDiff . toWeekly <$> readCounts fp

drawWeeklyDiffSemilog :: Field -> Counts -> IO ()
drawWeeklyDiffSemilog f cs = drawGraph f 0x0000ff 5
	$ (countToSemilog (read "2020-01-01", read "2020-05-01") (100, 500) (1, 18000) (400, 100) <$>) . filter ((/= 0) . snd) . mkDiff $ toWeekly cs

model :: Floating a => Day -> a
model d = 2 ** (fromIntegral (d `diffDays` fromGregorian 2020 1 2) / 8)

mkModel :: Floating a => Day -> Day -> [(Day, a)]
mkModel d0 d1 | d0 > d1 = []
mkModel d0 d1 = (d0, model d0) : mkModel (addDays 1 d0) d1

drawCounts :: Field -> Counts -> IO ()
drawCounts f cs = drawGraph f 0x0000ff 5 $ countToPoint (read "2020-01-01", read "2020-05-01") (100, 500) (0, 18000) (400, 100) <$> cs

drawSemilog :: Field -> Counts -> IO ()
drawSemilog f cs = drawGraph f 0x0000ff 5
	$ countToSemilog (read "2020-01-01", read "2020-05-01") (100, 500) (1, 18000) (400, 100) <$> cs

drawWithModel :: IO Field
drawWithModel = do
	f <- openField "foo" [exposureMask]
	drawCounts f $ mkModel (read "2020-01-02") (read "2020-04-24")
	drawCountsFromFile f "data.txt"
	drawScale f 0
	drawScale f 5000
	drawScale f 10000
	drawScale f 15000
	drawScale f 20000
	drawDayScale f $ read "2020-01-15"
	drawDayScale f $ read "2020-02-15"
	drawDayScale f $ read "2020-03-15"
	drawDayScale f $ read "2020-04-15"
	waitExposure f
	pure f

drawSemilogWithModel :: IO Field
drawSemilogWithModel = do
	f <- openField "foo" [exposureMask]
	drawSemilog f $ mkModel (read "2020-01-02") (read "2020-04-24")
	drawSemilogFromFile f "data.txt"
	drawSemilogScale f 1
	drawSemilogScale f 10
	drawSemilogScale f 100
	drawSemilogScale f 1000
	drawSemilogScale f 10000
	drawDayScale f $ read "2020-01-15"
	drawDayScale f $ read "2020-02-15"
	drawDayScale f $ read "2020-03-15"
	drawDayScale f $ read "2020-04-15"
	waitExposure f
	pure f

drawWeeklyDiffSemilogWithModel :: IO Field
drawWeeklyDiffSemilogWithModel = do
	f <- openField "foo" [exposureMask]
	drawWeeklyDiffSemilog f $ mkModel (read "2020-01-02") (read "2020-04-24")
	drawWeeklyDiffSemilogFromFile f "data.txt"
	drawSemilogScale f 1
	drawSemilogScale f 10
	drawSemilogScale f 100
	drawSemilogScale f 1000
	drawSemilogScale f 10000
	drawDayScale f $ read "2020-01-15"
	drawDayScale f $ read "2020-02-15"
	drawDayScale f $ read "2020-03-15"
	drawDayScale f $ read "2020-04-15"
	waitExposure f
	pure f

drawWeeklyDiffWithModel :: IO Field
drawWeeklyDiffWithModel = do
	f <- openField "foo" [exposureMask]
	drawWeeklyDiff f $ mkModel (read "2020-01-02") (read "2020-04-24")
	drawWeeklyDiffFromFile f "data.txt"
	drawScale' f 0
	drawScale' f 2500
	drawScale' f 5000
	drawScale' f 7500
	drawScale' f 10000
	drawDayScale f $ read "2020-01-15"
	drawDayScale f $ read "2020-02-15"
	drawDayScale f $ read "2020-03-15"
	drawDayScale f $ read "2020-04-15"
	waitExposure f
	pure f

drawDiffWithModel :: IO Field
drawDiffWithModel = do
	f <- openField "foo" [exposureMask]
	drawDiff f $ mkModel (read "2020-01-02") (read "2020-04-24")
	drawDiffFromFile f "data.txt"
	drawScale'' f 0
	drawScale'' f 500
	drawScale'' f 1000
	drawScale'' f 1500
	drawScale'' f 2000
	drawScale'' f 2500
	drawDayScale f $ read "2020-01-15"
	drawDayScale f $ read "2020-02-15"
	drawDayScale f $ read "2020-03-15"
	drawDayScale f $ read "2020-04-15"
	waitExposure f
	pure f

drawLineP :: Field -> Point -> Point -> IO ()
drawLineP f (Point x1 y1) (Point x2 y2) = drawLine f 0xffffff 2 x1 y1 x2 y2

drawScale :: Field -> Double -> IO ()
drawScale f c = do
	drawLineP f
		(countToPoint (read "2020-01-01", read "2020-05-01") (100, 500) (0, 18000) (400, 100) (read "2020-05-05", c))
		(countToPoint (read "2020-01-01", read "2020-05-01") (100, 500) (0, 18000) (400, 100) (read "2020-05-11", c))
	drawStr f 0xffffff "sans" 12 x (y + 6) (show c)
	where
	Point x y =
		(countToPoint (read "2020-01-01", read "2020-05-01") (100, 500) (0, 18000) (400, 100) (read "2020-05-15", c))

drawScale' :: Field -> Double -> IO ()
drawScale' f c = do
	drawLineP f
		(countToPoint (read "2020-01-01", read "2020-05-01") (100, 500) (0, 8000) (400, 100) (read "2020-05-05", c))
		(countToPoint (read "2020-01-01", read "2020-05-01") (100, 500) (0, 8000) (400, 100) (read "2020-05-11", c))
	drawStr f 0xffffff "sans" 12 x (y + 6) (show c)
	where
	Point x y =
		(countToPoint (read "2020-01-01", read "2020-05-01") (100, 500) (0, 8000) (400, 100) (read "2020-05-15", c))

drawScale'' :: Field -> Double -> IO ()
drawScale'' f c = do
	drawLineP f
		(countToPoint (read "2020-01-01", read "2020-05-01") (100, 500) (0, 1500) (400, 100) (read "2020-05-05", c))
		(countToPoint (read "2020-01-01", read "2020-05-01") (100, 500) (0, 1500) (400, 100) (read "2020-05-11", c))
	drawStr f 0xffffff "sans" 12 x (y + 6) (show c)
	where
	Point x y =
		(countToPoint (read "2020-01-01", read "2020-05-01") (100, 500) (0, 1500) (400, 100) (read "2020-05-15", c))

drawSemilogScale :: Field -> Double -> IO ()
drawSemilogScale f c = do
	drawLineP f
		(countToSemilog (read "2020-01-01", read "2020-05-01") (100, 500) (1, 18000) (400, 100) (read "2020-05-05", c))
		(countToSemilog (read "2020-01-01", read "2020-05-01") (100, 500) (1, 18000) (400, 100) (read "2020-05-11", c))
	drawStr f 0xffffff "sans" 12 x (y + 6) (show c)
	where
	Point x y =
		(countToSemilog (read "2020-01-01", read "2020-05-01") (100, 500) (1, 18000) (400, 100) (read "2020-05-15", c))

countLine :: Field -> (Day, Double) -> (Day, Double) -> IO ()
countLine f c1 c2 = drawLineP f
	(countToPoint (read "2020-01-01", read "2020-05-01") (100, 500) (0, 18000) (400, 100) c1)
	(countToPoint (read "2020-01-01", read "2020-05-01") (100, 500) (0, 18000) (400, 100) c2)

drawDayScale :: Field -> Day -> IO ()
drawDayScale f d = do
	countLine f (d, - 800) (d, - 1500)
	drawStr f 0xffffff "sans" 10 (x - 12) y (show d)
	where
	Point x y =
		(countToPoint (read "2020-01-01", read "2020-05-01") (100, 500) (0, 18000) (400, 100) (d, - 2500))
